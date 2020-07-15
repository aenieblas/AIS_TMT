# filter_functions.R
# Anne-Elise Nieblas
# 11/11/2019
# Description: Reformating of scripts developed for Nieblas et al. 2019 (Seychelles case study in the FAO AIS Atlas)
# Functions : overland, remove_ports, remove_dupes, impos_positions

# install.packages('pacman')
# install.packages("remotes")
# remotes::install_github("Displayr/flipTime")
# library(pacman)
# p_load('sp','maptools','remotes','flipTime','raster','geosphere')


overland<-function(dataset,create_column=TRUE,create_plot=FALSE,remove_points=FALSE){
  #' @name overland
  #' @title Define coordinates that lie overland
  #' @description Define coordinates of the input dataset that lie overland and add a new column where TRUE==LAND and FALSE==WATER
  #' @param dataset dataframe ran thru dataset_prep.R containing: vesselmap, mmsi, time,lat and lon, type = data.frame;
  #' @param create_column whether to add a column that indicates whether a point is over land or not, defaults to TRUE, type = boolean;
  #' @param create_plot whether to produce a map of the results, defaults to FALSE, type = boolean;
  #'
  #' @return Return a dataframe with a new column identifying if the point is overland 
  #' 
  #' @author Anne-Elise Nieblas \email{anne.elise.nieblas@gmail.com}
  #' @keywords preprocessing, preparation, spatio-temporal resolution
  #'
  #' @usage dataset <- overland(dataset_matched)
  
  # library(sp)
  # library(maptools)
  data(wrld_simpl)
  # data(wrldHiRes)
  
  ## Create a SpatialPoints object
  set.seed(0)
  points<-as.data.frame(cbind(as.numeric(dataset$Longitude),as.numeric(dataset$Latitude)))
  L1<-dim(dataset)[1]
  dataset<-dataset[complete.cases(points),]
  L2<-dim(dataset)[1]
  print(paste0(L1-L2,' static data rows removed'))
  points<-points[complete.cases(points),]
  pts <- SpatialPoints(points, proj4string=CRS(proj4string(wrld_simpl)))
  
  ## Find which points fall over land
  ii <- !is.na(over(pts, wrld_simpl)$FIPS)
  if (create_column==TRUE){
    dataset$overland<-ii
  }
  if(remove_points==TRUE){
    o_dim<-dim(dataset)[1]
    if(length(which(ii)==TRUE)>0){dataset<-dataset[-which(ii==TRUE),]}else{dataset<-dataset}
    print(paste('number of points over land that were deleted : ',length(which(ii==TRUE)),' out of ',o_dim,sep=''))
  }
  
  if(create_plot==TRUE){
    ## Check that it worked
    xlim<-as.numeric(range(dataset$Longitude))
    ylim<-as.numeric(range(dataset$Latitude,na.rm=T))
    plot(pts, col=1+ii, pch=16,lwd=.5)
    map(wrld_simpl,xlim=c(xlim[1]+1, xlim[2]-1),ylim=c(ylim[1]+1, ylim[2]-1),add=TRUE)
    legend('bottomleft',legend=c('points at sea',paste0(length(which(ii==TRUE)),' points overland')),col=c(1,2),pch=16)
    # points(pts, col=1+ii, pch=16,lwd=.5)
    # points(pts[which(ii==TRUE)],pch=16,col='blue')
  }
  return(dataset)
  
  }

# identifies a buffer of X meters around a port, and removes these points.
remove_ports<-function(dataset,file,width){
  #' @name remove_ports
  #' @title Define coordinates that lie within a buffer (width) around a defined port
  #' @description Defines and removes coordinates of the input dataset that lie within a buffer (width) around a defined port
  #' @param dataset dataframe ran thru dataset_prep.R containing: vesselmap, mmsi, time,lat and lon, type = data.frame;
  #' @param width the buffer around a port coordinates within which to search for points. units=meters, type = numeric;
  #'
  #' @return Return a dataframe with port coordinates removed
  #' 
  #' @author Anne-Elise Nieblas \email{anne.elise.nieblas@gmail.com}
  #' @keywords preprocessing, preparation, spatio-temporal resolution
  #'
  #' @usage dataset <- remove_ports(dataset,width=10000)
  
  # library(raster)
  # library(sp)
  ports<-read.csv(file,header=TRUE,sep=',')
  
  rst<-data.frame(lon=ports$londec,lat=ports$latdec)
  coordinates(rst) <- c("lon","lat")
  crs(rst)<-CRS('+proj=longlat +datum=WGS84')
  p_buff<-raster::buffer(rst, width=width)
  
  points<-data.frame(lon=as.numeric(dataset$Longitude),lat=as.numeric(dataset$Latitude))
  pts <- SpatialPoints(points,proj4string=CRS(proj4string(p_buff)))
  
  result <- as.integer(over(pts, p_buff))
  yes_port<-which(result==1)
  
  xlim<-as.numeric(range(dataset$Longitude,na.rm=T))
  ylim<-as.numeric(range(dataset$Latitude,na.rm=T))
  plot(pts, col=1, pch=16,lwd=.5)
  map(wrld_simpl,xlim=c(xlim[1]+1, xlim[2]-1),ylim=c(ylim[1]+1, ylim[2]-1),add=T,col='dark grey')
  points(pts, col='black', pch=16,)
  points(ports$londec,ports$latdec,pch=16,col='blue')
  points(dataset[yes_port,'Longitude'],dataset[yes_port,'Latitude'],pch=16,col='red')
  legend('bottomleft',legend=c('AIS points','Ports',paste0(length(yes_port),' points w/in 1km of port')),col=c(1,'blue',2),pch=16)
  
  # plot(pts,pch=1,col='black')
  # map('worldHires',add=T,col='dark grey')
  
  print(paste(length(yes_port),' positions out of ',dim(dataset)[1],' were removed that were within ',width,' meters of a port ',sep=''))
  if(length(yes_port)>0){dataset_port_remove<-dataset[-yes_port,]}else{dataset_port_remove<-dataset}
  
  }

impos_positions<-function(dataset){
  #' @name impos_positions
  #' @title Finds and removes impossible positions
  #' @description Finds and removes impossible positions (i.e., not on this earth) from a dataset
  #' @param dataset dataframe ran thru dataset_prep.R containing: vesselmap, mmsi, time,lat and lon, type = data.frame;
  #'
  #' @return Return a dataframe with impossible coordinates removed
  #' 
  #' @author Anne-Elise Nieblas \email{anne.elise.nieblas@gmail.com}
  #' @keywords preprocessing, preparation, spatio-temporal resolution
  #'
  #' @usage dataset <- impos_positions(dataset)
  
  toohighlat<-which(abs(as.numeric(dataset$Latitude))>=90)
  toohighlon<-which(abs(as.numeric(dataset$Longitude))>=180)
  print(paste('the number of too high latitudes that are removed ',length(toohighlat),sep=''))
  print(paste('the number of too high longitudes that are removed ',length(toohighlon),sep=''))
  
  if(length(toohighlat)>0){dataset<-dataset[-toohighlat,]}
  if(length(toohighlon)>0){dataset<-dataset[-toohighlon,]}
  return(dataset)
  }


#check for duplicates. duplicate is everything same except mmsi
doop_check<-function(dataset,colname_list=c('mmsi')){
  #' @name doop_check
  #' @title Finds and removes duplicate entries
  #' @description Finds and removes duplicated entries from a dataset
  #' @param dataset dataframe ran thru dataset_prep.R containing: vesselmap, mmsi, time,lat and lon, type = data.frame;
  #' @param colname_list column name that should NOT be checked for duplicates, e.g. mmsi, type = character;
  #'
  #' @return Return a dataframe with duplicate entries removed
  #' 
  #' @author Anne-Elise Nieblas \email{anne.elise.nieblas@gmail.com}
  #' @keywords preprocessing, preparation, spatio-temporal resolution
  #'
  #' @usage dataset <- doop_check(dataset,colname_list=c('mmsi'))
  
  # library(plyr)
  
  dataset<-ddply(dataset,.(StandardName), function(x) x[order(strptime(x$Timestamp,"%d/%m/%Y %H:%M:%OS")),])

  no_mmsi_df<-dataset[,-which(names(dataset) %in% colname_list)]
  dupes_mmsi<-which(duplicated(no_mmsi_df)==TRUE)
  #remove dupes
  print(paste(length(dupes_mmsi),' rows removed as complete duplicates of other rows out of ',dim(dataset)[1],sep=''))
  dataset[dupes_mmsi,]
  if(length(dupes_mmsi)>0){dataset<-dataset[-dupes_mmsi,]}
  return(dataset)
  }


### calculate speeds (distance/timediff) and removes outliers
## units to output: 'km/h','knots','m/s'
speed_calc<-function(dataset,units,gear,monitoring_system,effort=NA,too_close_dist=5,create_plot=FALSE){## speed = distance/time
  
  # make sure that dates are in order for each vessel
  dataset<-ddply(dataset,.(StandardName), function(x) x[order(strptime(x$Timestamp,"%d/%m/%Y %H:%M:%OS")),])
  d_dim=dim(dataset)[1]
  # library(flipTime)
  tt<-as.character(dataset$Timestamp)
  date_format_standard="%d/%m/%Y %H:%M:%OS"
  # tt<-format(AsDateTime(tt,us.format=FALSE), date_format_standard)
  # tt<-format(as.Date.POSIXlt(tt,us.format=FALSE), date_format_standard)
  tt<-format(ymd_hms(tt,tz='UTC'), date_format_standard)
  # xx<- format(anytime::anytime(df1[,1]), "%m/%d/%Y %H:%M")
  
  dataset$timestamp<-tt
  ## time
  #make two columns to compare times
  time_comp<-strptime(dataset$timestamp[1:(dim(dataset)[1]-1)],"%d/%m/%Y %H:%M:%OS")
  time_comp2<-strptime(dataset$timestamp[2:dim(dataset)[1]],"%d/%m/%Y %H:%M:%OS")
  timediff<-time_comp2-time_comp
  timediff<-c(NA,timediff)
  dataset$timediff<-timediff
  
  zerodiff<-which(dataset$timediff==0)
  print(paste0('there are ',length(zerodiff),' duplicate timestamps'))
  if(length(zerodiff)>0){
    # ## check that these are not in sequence
    for(z in 1:length(zerodiff)){tt[z]<-zerodiff[z]-zerodiff[(z+1)]}
    if(length(which(tt==(-1)))>0){print('SPEED CALC warning: zerodiffs are sequential. For logbooks, this means that there are >2 entries for one day.')}
    
    # if(monitoring_system!='LOG' | gear!='Purse seine'){
    # average the lat/lon of the zerodiff line with the previous line, and then delete the zerodiff line
    
    first<-zerodiff-1
    if(monitoring_system=='LOG'){
      # check that the catches are different and leave alone if they are.
      diff_catch<-NULL
      xxx=1
      for(z in 1:length(zerodiff)){
        diff_catch[xxx]=length(which(is.na(match(dataset[first[z],c('TotalCatch',effort)],dataset[zerodiff[z],c('TotalCatch',effort)]))==FALSE))
        # print(diff_catch[xxx])
        # if(diff_catch[xxx]>0){print(paste0(dataset[first[z],c('TotalCatch',effort)],' DOES EQUAL ',dataset[zerodiff[z],c('TotalCatch',effort)],collapse=','))}
        # if(diff_catch[xxx]==0){dataset<-dataset[-zerodiff[z],]}
        xxx=xxx+1
      }
      print(paste(length(which(diff_catch==2)),' of ',d_dim,' rows were deleted because the timediff'))
      
      dataset<-dataset[-which(diff_catch==2),]
    }else{
      
      for(z in 1:length(zerodiff)){ 
        dataset$Latitude<-as.numeric(dataset$Latitude);dataset$Longitude<-as.numeric(dataset$Longitude)
        dataset[first[z],'lat']<-mean(dataset[zerodiff[z],'Latitude'],dataset[first[z],'Latitude'],na.rm=TRUE)
        dataset[first[z],'lon']<-mean(dataset[zerodiff[z],'Longitude'],dataset[first[z],'Longitude'],na.rm=TRUE)
      }
      o_dim<-dim(dataset)[1]
      if(length(zerodiff)>0){dataset<-dataset[-zerodiff,]}
      print(paste(length(zerodiff),' of ',o_dim,' rows were deleted because the timediff from the previous row was 0, thus a duplicate
                 Lats and longs of previous rows were averaged with deleted row.'))
      
    }
  }
  
  
  
  ## distance
  pt1<-cbind(dataset$Longitude[1:(dim(dataset)[1]-1)],dataset$Latitude[1:(dim(dataset)[1]-1)])
  pt2<-cbind(dataset$Longitude[2:(dim(dataset)[1])],dataset$Latitude[2:(dim(dataset)[1])])
  dataset$distance_vincent<- c(NA,distVincentyEllipsoid(pt1,pt2))
  # dataset$distance<-c(NA,pointDistance(pt1,pt2,lonlat=TRUE))
  
  
  # for high-frequency data (i.e. VMS and AIS) find distances <too_close_dist (default = 5 m)
  if(monitoring_system!='LOG'){
    too_close<-which(dataset$distance<too_close_dist)
    # remove distances too_close
    print(paste0(length(too_close),' positions removed as they were <',too_close_dist,'m from previous position'))
    if(length(too_close)>0){dataset<-dataset[-too_close,]}
  }
  
  # speed calculation: distance (m)/time (s)
  dataset$speedcalc<-NA
  if(units=='knots'){
    dataset$speedcalc<-(dataset$distance/dataset$timediff)*1.94384 # m/s converted to knots
  }
  if(units=='km/h'){
    dataset$speedcalc<-(dataset$distance/dataset$timediff)*3.6 # m/s converted to km/h
  }
  if(units=='m/s'){
    dataset$speedcalc<-dataset$distance/dataset$timediff
  }
  negspeed<-which(dataset$speedcalc<0)
  # correct for changes in time-stamp between end of records for one vessel and the start of another
  if(length(negspeed)==length(unique(dataset$StandardName[negspeed]))){dataset[negspeed,c("distance","timediff","speedcalc")]<-NA}
  
  ## calculate and note speed outliers
  upperthreshold<-quantile(dataset$speedcalc,na.rm=TRUE)[4]+((quantile(dataset$speedcalc,na.rm=TRUE)[4]-quantile(dataset$speedcalc,na.rm=TRUE)[2])*1.5)
  lowerthreshold<-quantile(dataset$speedcalc,na.rm=TRUE)[2]-((quantile(dataset$speedcalc,na.rm=TRUE)[4]-quantile(dataset$speedcalc,na.rm=TRUE)[2])*1.5)
  
  if(gear=='Drifting longline' & units=='knots'){max_speed=13.5}
  if(gear=='Purse seine' & units=='knots'){max_speed=18}
  if(gear=='Supply vessel' & units=='knots'){max_speed=15}
  if(gear=='Trawl'& units=='knots'){max_speed=13}
  

  q999<-quantile(dataset$speedcalc,probs=0.99,na.rm=TRUE)
  print(summary(dataset$speedcalc))
  tt<-which(dataset$speedcalc>q999)
  if(create_plot==TRUE){
  hist(dataset$speedcalc[-tt],xlim=c(0,upperthreshold+1),breaks=10000,main='99th percentile of calculated speed',xlab=units)
  abline(v = max_speed, col = "blue", lwd = 2)
  abline(v = upperthreshold, col = "red", lwd = 2)
  legend('top',legend=c('gear threshold','IQR threshold'),col=c('blue','red'),pch=16)
  }
  
  ######## remove outliers ######
  i_out<-which(dataset$speedcalc>max_speed)
  
  # i_out<-which(dataset$speedcalc>upperthreshold | dataset$speedcalc<lowerthreshold)
  # major_outliers<-dataset[which(dataset$speedcalc>upperthreshold | dataset$speedcalc<lowerthreshold),]
  # print(paste(length(i_out),' rows of ',o_dim,' have been deleted as they fall above the IQR upper: ',upperthreshold,' ',units,sep=''))
  if(monitoring_system!='LOG'){
    if(length(i_out)>0){dataset<-dataset[-i_out,]}
    print(paste(length(i_out),' rows have been deleted as they fall above the max speed for ',gear,': ',max_speed,' ',units,sep=''))
    
  }
  
  dataset$timestamp<-as.POSIXct(strptime(as.character(dataset$timestamp),
                                         format = "%d/%m/%Y %H:%M:%S"),
                                format = "%d/%m/%Y %H:%M:%S")
  
  # dataset<-dataset[-i_out,]
  return(dataset)
  
}

# mode function to find value with highest number of occurrences
getmode <- function(v) {
       uniqv <- unique(na.exclude(v))
       uniqv[which.max(tabulate(match(v, uniqv)))]
  }
