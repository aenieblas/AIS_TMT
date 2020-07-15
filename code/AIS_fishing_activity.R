# AIS_fishing_activity.R
# Anne-Elise Nieblas
# 11/11/2019
# Description: This script prepares, filters, and uses raw AIS data to calculate the fishing activity 
#  within an area of interest following 8 steps:
#                1. Set up working environment 
#                2. Load and set up data 
#                3. Vessel verification
#                4. Data quality check
#                5. Define “Speed Rule”
#                6. Identify probable fishing events based on the Speed Rule
#                7. Effect of spatial resolution on calculations of fishing activity
#                8. Explore spatio-temporal patterns of fishing activity

# Inputs : 
# raw AIS data in the format as extracted by TMT Tracking 
#   col 1 : MMSI
#   col 2 : Timestamp
#   col 3 : Name
#   col 4 : CallSign
#   col 5 : ImoNumber
#   col 6 : Length
#   col 7 : Breadth
#   col 8 : DeviceClass
#   col 9 : ShipType
#   col 10: Destination
#   col 11: Draught
#   col 12: Latitude
#   col 13: Longitude
#   col 14: SOG (Speed over Ground)
#   col 15: COG (Course over Ground to the next waypoint)
#   col 16: NavStatus (0 = under way using engine; 1 = at anchor; 2 = not under command; 3 = restricted maneuverability;

# shapefiles of the region(s) of interest. Here, EEZ of Guinea Bissau and the Joint Marine Area of Senegal and Guinea Bissau


#  clear environment
rm(list=ls())
options(stringsAsFactors=FALSE)

# ---------------------------------------------- STEP A CUSTOMISATION ------------------------------------------

## define your home directory. copy the path to where the exercise file is stored on your computer
#     e.g., 'C:\AIS_workshop\exercise' in windows
#     e.g.  '/home/aen/AIS_workshop/exercise'

home<- '/home/ae/Documents/PERSONAL/COOOL/projects/TMTracking_Gambia/code/code_github/code_032020/'#'/home/ae/Documents/COOOL/projects/TMTracking_SRFC/exercise/'

## this 'year' variable is used to label plots and is inserted in file names to help track the data being analysed. 
#  can be customised to any text

year='2017_2019'

## Step 6 takes the filtered AIS data and identifies fishing events based on the speed rule. 
#     This can take considerable time, i.e. 30 min for this data set. We have already done this step and the data are available
#     in the data folder. 
#  
#     Only change to 'YES' if you have new data for which you would like to perform this step yourself. 

identify_fishing_events='NO' 

#--------------------------------------------------- STEP 1 SET UP WORKING ENVIRONMENT ---------------------------------------------------------------------------------

#   1.1 set working directory to home path defined above
setwd(home)
data_files<-list.files(paste0(home,'/data/raw_ais/'))

# double check that you are in the 'exercise' folder 
getwd()

#   1.3 load support functions
source(paste0(home,'/code/aoi.R'))
source(paste0(home,'/code/filter_functions.R'))
source(paste0(home,'/code/rasterise_fishing_per_km.R'))

#   1.4 load libraries
library(pacman)
p_load('lubridate','mapdata','maptools','sp','rgdal','plyr','raster','tidyr','geosphere','reshape2','marmap','ggplot2',
       'oceanmap','marmap','sf','rnaturalearth','rnaturalearthdata','gridExtra','tictoc','maps','remotes')
world <- ne_countries(scale = "medium", returnclass = "sf")




#--------------------------------------------------- STEP 2 LOAD AND SET UP DATA ---------------------------------------------------------------------------------

#     2.1 load and set up raw ais data
ais_data<-NULL
for (y in 1:length(data_files)){
  #     - raw ais data files (tab separated values)
  ais_data<-rbind(ais_data,read.delim(paste0(home,'/data/raw_ais/',data_files[y]),header=T,sep='\t',quote=''))
}
#     - remove rows of metadata
ais_data <- ais_data[-which(ais_data$Length=='Length'),]
original_data<-ais_data

#     - make separate columns for year, month and day based on the Timestamp column
ais_data$TimeStamp<-ais_data$Timestamp
ais_data<-separate(ais_data, "TimeStamp", c("Year", "Month", "Day"), sep = "-")
ais_data<-separate(ais_data, "Day", c("Day", "Time"), sep = " ")

#     2.2 load joint marine area shapefile
shp=paste0(home,'/data/jra/jra.shp')
jra <- readOGR(dsn = shp, stringsAsFactors = F)

#     2.3 load guinea bissau eez shapefile
eezshp=paste0(home,'/data/eez/eez.shp')
eez <- readOGR(dsn = eezshp, stringsAsFactors = F)

#     2.4 bathymetry of area - changing the parameters of this command can change the bathy file : 
#     getNOAA.bathy(lon1, lon2, lat1, lat2, resolution = 4)
#     resolution=300 (5 deg); resolution=60 (1 deg), resolution = 18 (0.3 deg)
bath<-getNOAA.bathy(-30,-5,7,15,resolution=18,keep=T)

bathy<-fortify(bath)
bathy[which(bathy$z>0),'z']<-0




#--------------------------------------------------- STEP 3 VESSEL VERIFICATION ---------------------------------------------------------------------------------

# 3.1 identify the vessels from the vessel ID information

#     - select rows with vessel information
vessel_info_rows <- which(ais_data$Length!="")
vessel_info <- ais_data[vessel_info_rows,]

#     - unique combinations of Name, CallSign and ImoNumber
vessel_id <- unique(vessel_info[,c('Year','MMSI','Name','CallSign','ImoNumber')])
write.csv(vessel_id,paste0(home,'data/vessel_id_',year,'.csv'),row.names = FALSE)

#     - **manually** cross-reference to identify which IMOs match the MMSIs which match the vessel Names and CallSigns
#     - vessel registeries were double-checked on MarineTraffic to identify vessel type (trawler), and matches between MMSI/IMO/vessel names
vessel_id_1 <- read.csv(paste0(home,'/data/vessel_id_',year,'_1.csv'),header = TRUE, stringsAsFactors = FALSE)
vessel_id_1

#     - number of vessels (number of unique standard vessel names)
num_vessels<-length(unique(vessel_id_1$StandardName))
num_vessels

#     - rename vessels to match the standard names
ais_data$StandardName<-''
for(sn in 1:dim(vessel_id_1)[1]){
  ais_data[which(ais_data$MMSI==vessel_id_1[sn,'MMSI'] ),'StandardName'] <-vessel_id_1[sn,'StandardName']
}




#--------------------------------------------------- STEP 4 DATA QUALITY ASSURANCE ---------------------------------------------------------------------------------

#     4.1 remove points over land and removes points without complete Lat/Long positions
ais_data <- overland(ais_data, create_plot=TRUE,remove_points=TRUE)
 
#     4.2 remove points within 1 km of port 
ais_data <- remove_ports(ais_data,file='data/ports_wafrica.csv',width=1000)

#     4.3 remove impossible points (outside of +/-180E/W +/-90N/S )
ais_data <- impos_positions(ais_data)

#     4.4 removes duplicate rows
ais_data<-doop_check(ais_data,colname_list=c('MMSI','Name','CallSign','ImoNumber','Length','Breadth','DeviceClass','ShipType',
                                             'Destination','Draught','NavStatus','StandardName'))

#     4.5 checks speed of trawler. Different from SOG: speed calculated as distance between sequential transmissions divided by time between sequential transmissions 
#         removes duplicate timestamps, points that are too close, and points that are too fast
ais_data<-speed_calc(ais_data,units='knots',gear='Trawl',monitoring_system='AIS',effort=NA,too_close_dist=5,create_plot=TRUE)

#     - replace negative time difference (timediff) with NA (negative times occur between different vessel records, generally at the start of the year)
ais_data[which(ais_data$timediff<0),'timediff']<-NA

#     - there are some points where > 24 hours passes between transmissions. 
#     - these can be considered as different trajectories, and the timediff here is set to NA
ais_data[which(ais_data$timediff/60>(60*24)),]<-NA

rownames(ais_data)<-seq(1,dim(ais_data)[1],by=1) # rename rows so that rownames are in sequential order (important for identifying fishing events in a later step)

#     - look at the time intervals between transmissions (timediff is in seconds, so divide by 60)
q95<-quantile(ais_data$timediff/60,probs=0.95,na.rm=T)
hist(ais_data$timediff/60,xlim=c(0,q95),breaks=10000,xlab='minutes',main='Time interval between transmissions')
summary(ais_data$timediff/60)




#--------------------------------------------------- STEP 5 SPEED RULE ---------------------------------------------------------------------------------

# 5. check the calculated speed and define the 'Speed Rule'
#    - calculated speed is used as the measure of fishing speed as it calculates the approximate speed between points rather than at one point in time as in SOG, and is a better indicator of the average vessel speed
no_zero<-ais_data[-which(as.numeric(ais_data$speedcalc)<0.2),] # remove zeros
hist(as.numeric(no_zero$speedcalc),breaks=1000,main='Calculated speed of trawlers, zeros removed', xlab='knots')
summary(no_zero$speedcalc)

#    - probable fishing speed range from the literature is between 2-8 knots (2.5-5.5 knots: de Souza et al. 2016; https://doi.org/10.1371/journal.pone.0163760)
# 2-8 knots: Mills et al. 2007; https://academic.oup.com/icesjms/article/64/2/248/2182309)
abline(v = 2, col = "red", lwd = 2)
abline(v = 8, col = "red", lwd = 2)
abline(v = 2.5, col = "magenta", lwd = 2)
abline(v = 5.5, col = "magenta", lwd = 2)

#     - probable fishing speed range based on 95% of the range from the literature, i.e. 2-8 (Mills et al. 2007)
#2017:  3.1-7.7 knots
quant<-quantile(as.numeric(ais_data[which(ais_data$speedcalc>=2 & ais_data$speedcalc<=8),'speedcalc']),na.rm=T,probs=c(0.025,0.975))
quant # this range indicates the 'Speed Rule'
abline(v = quant[1], col = "blue", lwd = 2)
abline(v = quant[2], col = "blue", lwd = 2)
legend('topright',legend=c('Speed rule (Mills et al. 2007)','Speed rule (de Souza et al. 2016)', paste0('Speed rule (',year,' data) :',round(quant[1],1),"-",round(quant[2],1)," knots")),col=c('red','magenta','blue'),pch=16)




#--------------------------------------------------- STEP 6 IDENTIFY PROBABLE FISHING EVENTS ---------------------------------------------------------------------------------


if(identify_fishing_events=='YES'){
# 6.1 identify probable (non)fishing events based on the speed rule and check against thresholds of fishing duration, 
#    distance over which vessel travels, and acceleration
#     - identify fishing events over multiple points and calculate the cumulative time and distance of each fishing event 
#     - this step takes a long time ~ 20 min with all 3 years of data
#     - uncomment if you want to perform this step by yourself. otherwise, we will load the data that has already been processed and saved previously

ais_data$fishing <- NULL
ais_data[which(ais_data$speedcalc>=quant[1] & ais_data$speedcalc<=quant[2]),'fishing']<-1 # probable fishing
ais_data[which(ais_data$speedcalc<quant[1] | ais_data$speedcalc>quant[2]),'fishing']<-0 # probable other activity

ev<-which(ais_data$fishing==1) # the rows of data where fishing probably occurs

#     - setup some new columns with default values
ais_data$event<-0
ais_data$cumtime<-NA
ais_data$cumtime[ev]<-ais_data$timediff[ev] ## assumes that fishing starts prior to the transmission point
ais_data$cumdist<-NA
ais_data$cumdist[ev]<-ais_data$distance_vincent[ev] ## assumes that fishing starts prior to the transmission point
ais_data$fishtime<-NA
ais_data$fishdist<-NA

    # - start counting fishing events from 1 (1-position events are not numerated)
tic()
f=1
for(e in 1:length(ev)){
  if(rownames(ais_data[ev[e+1],])==(as.numeric(rownames(ais_data[ev[e],]))+1)){
    ais_data$event[ev[e]]    <-f   # assign event number to each probable fishing point
    ais_data$event[ev[e+1]]  <-f # assign event number to each probable fishing point

    # calculates the cumulative time spent fishing (adds time from previous sequential probable fishing point to current probable fishing point)
    ais_data$cumtime[ev[e+1]]<-sum(ais_data$cumtime[ev[e]],ais_data$timediff[ev[e+1]])

    # calculate the cumulative distance of the fishing event
    ais_data$cumdist[ev[e+1]]<-sum(ais_data$cumdist[ev[e]],ais_data$distance_vincent[ev[e+1]])
  }else{
    ais_data$fishtime[ev[e]] <-ais_data$cumtime[ev[e]] # cumulative time at the end of the fishing event
    ais_data$fishdist[ev[e]] <-ais_data$cumdist[ev[e]] # cumulative distance at the end of the fishing event
    f<-f+1
  }
}
toc()
write.csv(ais_data,file=paste0('data/ais_data_fishing_events_',year,'.csv'),row.names = FALSE)
}

# load data with the fishing events already detected (to save time during the workshop as this step is long)
ais_data<-read.csv(file=paste0('data/ais_data_fishing_events_',year,'.csv'),header=T,stringsAsFactors = FALSE)

# plot of probable (non)fishing events - the plot is saved to the '/plots' folder
jpeg(paste0(home,'/plots/fishing_events_',year,'.jpg'))
map('worldHires',col='dark grey',xlim=c(-22,16.5),ylim=c(-25,52))
points(ais_data[ais_data$event==0,'Longitude'],ais_data[ais_data$event==0,'Latitude'],type='p',col='black')
points(ais_data[ais_data$event>0,'Longitude'],ais_data[ais_data$event>0,'Latitude'],type='p',pch=16,col='red')
legend('bottomleft',legend=c('probable fishing','non-fishing'),col=c('red','black'),pch=16,bty='n')
dev.off()

#     6.2 histogram of probable cumulated fishing duration with zeros removed 
#         - mid-water trawls can be opportunistic and range from 15min to 1 hour if there are concentrated schools of fish;
#         - multi-hour trawls occur when fish are distributed more broadly (Misund et al. 1999, https://academic.oup.com/icesjms/article/56/3/275/623497)
q95<-quantile(ais_data[which(ais_data$event>0),'fishtime']/3600,probs=0.95,na.rm=T) 
hist(ais_data[which(ais_data$event>0),'fishtime']/3600,breaks=1000,xlab='hours',xlim=c(0,q95),main='Duration of probable fishing events')
summary(ais_data[which(ais_data$event>0),'fishtime']/3600)
q95
#     - remove points where cumulative time is > q95
ais_data<-ais_data[-which(ais_data$cumtime/3600>q95),]


#     6.3 histogram of probable fishing distance with zeros removed
q95<-quantile(ais_data[which(ais_data$fishdist>0),'fishdist']/1000,probs=0.95,na.rm=T) 
hist(ais_data[which(ais_data$fishdist>0),'fishdist']/1000,breaks=100,xlab='km',xlim=c(0,q95),main='distance travelled during probable fishing events')
summary(ais_data[which(ais_data$fishdist>0),'fishdist']/1000)


#     6.4 acceleration is calculated as nautical mile/hour divided by hour = nautical mile/hour^2
#         - this is to check that the vessels are not accelerating significantly during fishing
acceleration <- ais_data$distance_vincent/(ais_data$timediff)^2 # m/s2 between each point
q95<-quantile(acceleration,probs=.95,na.rm=T)
hist(acceleration,breaks=10000,xlim=c(0,q95),main='acceleration',xlab='m/s^2')
hist(acceleration[which(ais_data$fishing==1)],breaks=10000,col='red',add=T)
legend('topright',legend=c('all points','prob fishing points'),col=c('black','red'),pch=c(1,16))

#     - save the fully filtered and processed ais_data as a .csv
write.csv(ais_data,file=paste0(home,'/data/ais_data_filtered_',year,'.csv'),row.names = FALSE)


#--------------------------------------------------- STEP 7 TEST EFFECT OF SPATIAL RESOLUTION ------------------------------------------------------------------

# 7. find the trawl fishing activity (number of hours) in a X°xX° cell (i.e. per km2)
# 7.1 test the effects of spatial resolution on apparent fishing activity

#------------- uncomment the line below and start from there if you must restart R session
# ais_data<-read.csv(paste0(home,'ais_data_filtered_',year,'.csv'), stringsAsFactors = FALSE,header=T)

fishing<-ais_data[which(ais_data$event>0),] # reduce data frame to points of probable fishing 

gridsize_list=c(1,0.5,0.1,0.05,0.04,0.03,0.02,0.015,0.01) # a list of gridsizes (in degrees) over which to test the effect of resolution on apparent fishing activity
time_unit='year' # unit of time over which to measure fishing activity

xlim_GB=c(-20.5,-15) # reduce spatial limits to those around the area of interest (here - the joint maritime zone of GB and Senegal)
ylim_GB=c(8,12.4)

#     7.1 for each grid size, we calculate the sum of the fishing activity within each cell, using the function "fishing_per_km2"
max_activity_per_gridsize<-NULL
for(gs in 1:length(gridsize_list)){
  fish_df<-NULL
  # using the function "fishing_per_km2" and selecting ggplot_object=F, we output a dataframe of the calculated fishing activity
  fish_df<-fishing_per_km2(fishing,time_unit=time_unit,gridsize=gridsize_list[gs],xlim_GB,ylim_GB,Zlim=c(0,5),ggplot_object=F,create_plot = F)
  max_activity_per_gridsize<-c(max_activity_per_gridsize,max(fish_df$Hrpkm2,na.rm=T))
}

# plot the maximum fishing activity per grid cell size to investigate the effect of spatial resolution on fishing activity calculations
plot(log10(gridsize_list),max_activity_per_gridsize,type='b',col='black',pch=16,
     xlab='Grid cell size (degrees, x axis has been logged)',
     ylab=paste0('Maximum fishing activity recorded (Hours/km^2*',time_unit,')'),axes=F,
     main='Effect of spatial resolution on calculations of fishing activity')
axis(1, at = log10(gridsize_list), labels = gridsize_list)
axis(2, at = seq(0,max(max_activity_per_gridsize,na.rm=T),by=5), labels = seq(0,max(max_activity_per_gridsize,na.rm=T),by=5))

# using the function "fishing_per_km2" and selecting ggplot_object=T, we output a ggplot object 
# create plots of fishing activity for different grid cell sizes
g1<-fishing_per_km2(fishing,time_unit=time_unit,gridsize=gridsize_list[1],xlim_GB,ylim_GB,Zlim=c(0,5),ggplot_object=T,create_plot = F)
g2<-fishing_per_km2(fishing,time_unit=time_unit,gridsize=gridsize_list[2],xlim_GB,ylim_GB,Zlim=c(0,5),ggplot_object=T,create_plot = F)
g3<-fishing_per_km2(fishing,time_unit=time_unit,gridsize=gridsize_list[3],xlim_GB,ylim_GB,Zlim=c(0,5),ggplot_object=T,create_plot = F)
g4<-fishing_per_km2(fishing,time_unit=time_unit,gridsize=gridsize_list[4],xlim_GB,ylim_GB,Zlim=c(0,5),ggplot_object=T,create_plot = F)
g5<-fishing_per_km2(fishing,time_unit=time_unit,gridsize=gridsize_list[length(gridsize_list)],xlim_GB,ylim_GB,Zlim=c(0,5),ggplot_object=T,create_plot = F)

# plot the separate plots as subplots
# subplot titles are the length of the grid cell in degrees
grid.arrange(g1,g2,g3,g4,g5)



#--------------------------------------------------- STEP 8 INVESTIGATE SPATIAL-TEMPORAL PATTERNS ------------------------------------------------------------------

# 8. investigate spatial-temporal patterns of trawl fishing activity - climatologies

# 8.1 calculate the mean monthly (climatology) of fishing activity (hours per km2) 
temp_gridsize=.1 # ~ 10 km. This is ~ the median distance over which fishing events occur.


#   8.1.1 all points, i.e. expand the xlim and ylim
#   - find the sum of hours in each grid for each month
month_all<-ddply(fishing,.(Month),function(x) fishing_per_km2(x,gridsize=temp_gridsize,time_unit=time_unit, Xlim=c(min(x$Longitude),max(x$Longitude)), 
                                                              Ylim=c(min(x$Latitude),max(x$Latitude)),ggplot_object=F,create_plot = F))
head(month_all)
#   - find the  mean of hours/km2 over all the points
month_all_ts<-ddply(month_all,.(Month),function(x) data.frame(MonthMean=mean(x$Hrpkm2,na.rm=T)))

plot(1:12,month_all_ts$MonthMean,type='b',pch=16,col='black',
     main=paste0('Monthly mean of fishing activity for ',year),
     xlab='Months',
     ylab='Fishing activity (hours/km2*yr)')
legend('top',legend=c('All probable fishing events'),col='black',pch=16,bty='n')



#   8.1.2 within the GB EEZ
#    - extract points within the area of interest (GB EEZ)
eez_fishing<-aoi(dataset=fishing,shp=paste0(home,'data/eez/eez.shp'),shp_type='eez',create_plot=TRUE,remove_points=TRUE)


#   - find the sum of hours in each grid for each month
month_eez<-ddply(as.data.frame(eez_fishing),.(Month),function(x) fishing_per_km2(x,time_unit=time_unit,gridsize=temp_gridsize,Xlim=c(xlim_GB),Ylim=c(ylim_GB),
                                                                                 ggplot_object=F,create_plot = F))
head(month_eez)
#   - find the  mean of hours/km2 over all the points
month_eez_ts<-ddply(month_eez,.(Month),function(x) data.frame(MonthMean=mean(x$Hrpkm2,na.rm=T)))


plot(1:12,month_all_ts$MonthMean,type='b',pch=1,col='black',
     main=paste0('Monthly mean of fishing activity for ',year),
     ylim=c(0,max(month_eez_ts$MonthMean,month_all_ts$MonthMean,na.rm=T)),
     xlab='Months',
     ylab='Fishing activity (hours/km2*yr)')
lines(1:12,month_eez_ts$MonthMean,pch=16,col='dark grey',type='b')
legend('top',legend=c('All probable fishing events','Probable fishing events in GB EEZ'),col=c('black','dark grey'),pch=c(1,16),bty='n')


#   8.1.3 within the JMA
#    - extract points within the area of interest (JMA)
jma_fishing<-aoi(dataset=fishing,shp=paste0(home,'/data/jra/jra.shp'),shp_type='eez',create_plot=TRUE,remove_points=TRUE)


#   - find the sum of hours in each grid for each month
month_jma<-ddply(as.data.frame(jma_fishing),.(Month),function(x) fishing_per_km2(x,time_unit=time_unit,gridsize=temp_gridsize,Xlim=c(xlim_GB),Ylim=c(ylim_GB),Zlim=c(0,0.3),
                                                                                 ggplot_object=F,create_plot = F))
head(month_jma)

#   - find the  mean of hours/km2 over all the points
month_jma_ts<-ddply(month_jma,.(Month),function(x) data.frame(MonthMean=mean(x$Hrpkm2,na.rm=T)))

# plot all the different time series together
plot(month_all_ts$MonthMean,type='b',pch=1,col='black',
     main=paste0('Monthly mean of fishing activity for ',year),
     xlab='Months',
     ylab='Fishing activity (hours/km2*yr)',ylim=c(0,max(month_jma_ts$MonthMean,month_eez_ts$MonthMean,month_all_ts$MonthMean,na.rm=T)))
lines(month_eez_ts$MonthMean,pch=16,col='dark grey',type='b')
lines(month_jma_ts$MonthMean,pch=16,col='orange',type='b')
legend('top',legend=c('All probable fishing events','Probable fishing events in GB EEZ','Probable fishing events in JMA'),
       col=c('black','dark grey','orange'),pch=c(1,16,16),bty='n')


# 8.2 Monthly spatial climatologies
#     - difficult to visualise the activity at very high resolutions for few data (i.e. data per month)

# define desired gridsize in degrees
month_gridsize=.1

# calculate the fishing activity / km2 for each month
# month_df<-ddply(fishing,.(Month),function(x) rasterise_fishing(x,gridsize=month_gridsize, xlim=xlim_GB, ylim=ylim_GB, whichfunction=sum))
month_df<-ddply(as.data.frame(fishing),.(Month),function(x) fishing_per_km2(x,time_unit=time_unit,gridsize=temp_gridsize,Xlim=c(xlim_GB),Ylim=c(ylim_GB),
                                                                            ggplot_object=F,create_plot = F))
# plot the fishing activity / km2 for each month
g_month<-ggplot(data = world) +
  geom_tile(data=month_df, aes(x=Longitude,y=Latitude,fill=Hrpkm2))+
  facet_wrap(~Month)+ 
  geom_polygon(data = eez, aes(x = long, y = lat, group = group), colour = "dark grey", fill = NA)+
  geom_sf(color = "grey", fill = "lightgrey")+
  coord_sf(xlim=xlim_GB,ylim=ylim_GB, expand = TRUE)+
  geom_contour(data=bathy,aes(x=x,y=y,z=z),
               breaks=c(-50),
               colour="dark grey", size=0.5  )+
  geom_polygon(data = jra, aes(x = long, y = lat, group = group), colour = "orange", fill = NA)+
  scale_fill_continuous(type='viridis',na.value='white')
g_month



#   8.3 Monthly mean time series
#     - 8.3.1 All points - month AND year
monthly_all<-ddply(fishing,.(Year,Month),function(x) fishing_per_km2(x,time_unit=time_unit,gridsize=temp_gridsize, Xlim=c(min(x$Longitude),max(x$Longitude)), 
                                                              Ylim=c(min(x$Latitude),max(x$Latitude)),ggplot_object=F,create_plot = F))
head(month_all)
#   - find the  mean of hours/km2 over all the points
monthly_all_ts<-ddply(monthly_all,.(Year,Month),function(x) data.frame(MonthMean=mean(x$Hrpkm2,na.rm=T)))

#     - 8.3.2 within the GB EEZ - month AND year
#       - find the sum of hours in each grid for each month and year
monthly_eez<-ddply(as.data.frame(eez_fishing),.(Year,Month),function(x) fishing_per_km2(x,time_unit=time_unit,gridsize=temp_gridsize,Xlim=c(xlim_GB),Ylim=c(ylim_GB),
                                                                                 ggplot_object=F,create_plot = F))
head(monthly_eez)
#   - find the  mean of hours/km2 over all the points
monthly_eez_ts<-ddply(monthly_eez,.(Year,Month),function(x) data.frame(MonthMean=mean(x$Hrpkm2,na.rm=T)))

#     - 8.3.3 within the JMA - month AND year
#       - find the sum of hours in each grid for each month and year
monthly_jma<-ddply(as.data.frame(jma_fishing),.(Year,Month),function(x) fishing_per_km2(x,time_unit=time_unit,gridsize=temp_gridsize,Xlim=c(xlim_GB),Ylim=c(ylim_GB),Zlim=c(0,0.3),
                                                                                 ggplot_object=F,create_plot = F))
head(monthly_jma)
#   - find the  mean of hours/km2 over all the points
monthly_jma_ts<-ddply(monthly_jma,.(Year,Month),function(x) data.frame(MonthMean=mean(x$Hrpkm2,na.rm=T)))


# fill the blank months with NAs
if(length(unique(na.exclude(monthly_jma_ts$MonthMean)))<dim(monthly_all_ts)[1]){ # if there are months with missing data
  tmp<-data.frame(Month=1:dim(monthly_all_ts)[1],MonthMean=NA)# make an empty data frame of the right size
  for(m in 1:dim(monthly_all_ts)[1]){  
  if(length(monthly_jma_ts[which(as.numeric(monthly_jma_ts$Month)==monthly_all_ts$Month[m] & 
                                 as.numeric(monthly_jma_ts$Year)==monthly_all_ts$Year[m]),'MonthMean'])>0){# if there is a value, put it on the right line
    tmp[m,c('Year','Month','MonthMean')]<-monthly_jma_ts[which(as.numeric(monthly_jma_ts$Month)==monthly_all_ts$Month[m] & 
                                                                 as.numeric(monthly_jma_ts$Year)==monthly_all_ts$Year[m]),c('Year','Month','MonthMean')]
    }}
monthly_jma_ts<-tmp}

#   - set up x axis values
xx<-seq(as.Date("2017/1/1"), by = "month", length.out = 36)
xx <- format(xx, "%m/%y")
xx=xx[1:34] # november and december of 2019 are missing

# plot all the different time series together
plot(monthly_all_ts$MonthMean,type='b',pch=1,col='black',
     main=paste0('Monthly fishing activity for ',year),
     axes=F,
     ylab='Fishing activity (hours/km2*yr)',ylim=c(0,max(monthly_jma_ts$MonthMean,monthly_eez_ts$MonthMean,monthly_all_ts$MonthMean,na.rm=T)))
axis(1, at = 1:34, labels = xx)
axis(2, at = seq(0,max(monthly_jma_ts$MonthMean,monthly_eez_ts$MonthMean,monthly_all_ts$MonthMean,na.rm=T),by=0.05), 
     labels = seq(0,max(monthly_jma_ts$MonthMean,monthly_eez_ts$MonthMean,monthly_all_ts$MonthMean,na.rm=T),by=0.05))
lines(monthly_eez_ts$MonthMean,pch=16,col='dark grey',type='b')
lines(monthly_jma_ts$MonthMean,pch=16,col='orange',type='b')
legend('top',legend=c('All probable fishing events','Probable fishing events in GB EEZ','Probable fishing events in JMA'),
       col=c('black','dark grey','orange'),pch=c(1,16,16),bty='n')





