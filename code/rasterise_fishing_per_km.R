## rasterise_fishing_per_km.R


rasterise_fishing <- function(dataset, gridsize=1, xlim, ylim, columnname='timediff',whichfunction=mean){
  ## Anne-Elise Nieblas
  ## 13/11/2019
  ## Description : Function to convert a data frame to a raster (gridded output)
  ## Output   : Raster object
  ## Requires : dataset (data frame), including columns "timediff" in seconds, "Longitude", "Latitude", and the specified variable
  ##            gridsize (numeric) : the size of the cell over which to rasterise, e.g. 1 for 1 degree by 1 degree
  ##            xlim (numeric range) : minimum and maximum values of the grid along the xaxis
  ##            ylim (numeric range) : minimum and maximum values of the grid along the yaxis
  ##            variable (character) : colunmn name upon which to perform the function
  ##            whichfunction ()     : function to apply
  
  library(raster)
  # mapping the total effort 
  rst <- data.frame(effort=eval(parse(text=paste("dataset$",columnname)))/3600,lon=dataset$Longitude,lat=dataset$Latitude)
  rst <- rst[complete.cases(rst),]
  coordinates(rst) <- c("lon","lat")
  
  # define the grid (here is where you can define the X of X°xX°)
  gridsize <- gridsize
  rx <- xlim   # Extent of map along the x-axis (longitude)
  nbx <- length(seq(rx[1], rx[2], by=gridsize))
  ry <- ylim   # Extent of map along the x-axis (latitude)
  nby <- length(seq(ry[1], ry[2], by=gridsize))  
  r <- raster(extent(rx, ry), nby, nbx)
  r[]<-1:ncell(r)
  
  
  
  rastermap <- raster2matrix(rasterize(rst,r,'effort',fun=whichfunction ))
  rownames(rastermap)<-seq(rx[1], rx[2], by=gridsize)
  colnames(rastermap)<-seq(ry[1], ry[2], by=gridsize)
  rasterdf<-melt(rastermap,varnames=c('Longitude','Latitude'), value.name = "Hours")
  
  return(rasterdf)
}




fishing_per_km2<-function(dataset,Xlim,Ylim,Zlim=c(0,max(gfdf$Hrpkm2,na.rm=T)),gridsize,time_unit,ggplot_object=F,create_plot=F){
  # Anne-Elise Nieblas
  # 13/11/2019
  # @Description: Function to calculate the fishing activity in a given grid cell and convert it to X/km2. 
  #              Allows for ggplot objects to be created for plotting now or in an arranged grid. 
  #              Note: the smaller the gridsize the bigger the dataframe - big dataframes can be heavy for your system and may crash Rstudio.
  # @Inputs: dataset (dataframe) : dataframe of AIS data for probable fishing points only. 
  #          gridsize (numeric)  : size of the grid in degrees
  #          xlim (numeric range): range of longitudes of the grid
  #          ylim (numeric range): range of latitudes of the grid
  #          zlim (numeric range): range of values for the colorbar of the plot
  #          ggplot_object (boolean): the choice of the object to return is either the gridded dataframe (default), 
  #                                   or the ggplot_object (i.e. ggplot_object = T) 
  #          plot_create (boolean): if TRUE, the function will output the plot for the gridsize defined
  
  library(pacman)
  p_load('mapdata','maptools','sp','rgdal','plyr','flipTime','raster','tidyr','geosphere','reshape2','marmap','ggplot2',
         'oceanmap','marmap','sf','rnaturalearth','rnaturalearthdata','gridExtra')
  world <- ne_countries(scale = "medium", returnclass = "sf")
  # resolution=300 (5 deg); resolution=60 (1 deg), resolution = 18 (0.3 deg)
  bath<-getNOAA.bathy(-30,-11,4,22,resolution=6,keep=T) ## <------------------------ AE generalise this 
  bathy<-fortify(bath)
  bathy[which(bathy$z>0),'z']<-0
  
  Xlim=Xlim # reduce spatial limits to those around the area of interest (here - the joint maritime zone of GB and Senegal)
  Ylim=Ylim
  

  gridsize=gridsize # a list of gridsizes (in degrees) over which to test the effect of resolution on apparent fishing activity

# for each grid size, we calculate the sum of the fishing activity within each cell
# for(gs in 1:length(gridsize_list)){
  # gridsize=gridsize_list[gs]                 # choose one grid size
  
  gfdf<-NULL                                 # predefine an empty dataframe to be filled
  
  # calculate length (in km) of one side of the grid at the latitude x
  x=mean(Xlim,na.rm=T)                       # degree latitude in center of area of interest
  p1<-c(x,Ylim[1])                                 # lat/lon at the start of the grid
  p2=c(x-gridsize,Ylim[1])                         # lat/lon 1 gridsize away from start of grid.
  km2<-(distVincentyEllipsoid(p1,p2)/1000)^2 # calculate the distance
  
  # time unit
  if(time_unit=='year'){timetotal<-length(unique(dataset$Year))  }
  if(time_unit=='month'){timetotal<-length(unique(dataset[,c('Year','Month')]))}
  
  
  # sum the 
  gridded_fishing <- rasterise_fishing(dataset,gridsize=gridsize, xlim=Xlim, ylim=Ylim, whichfunction=sum)
  gridded_fishing$Hrpkm2<-gridded_fishing$Hours/(km2*timetotal)
  gfdf<-rbind(gfdf,cbind(gridsize=gridsize,gridded_fishing))
  
  Zlim=Zlim
  
  if(create_plot==T|ggplot_object==T){
    gfdf[which(gfdf$Hrpkm2>Zlim[2]),'Hrpkm2']<-Zlim[2]
  }
  
 
  g<-ggplot(data = world) +
    # geom_tile(data = bathy, aes(x=x,y=y,fill=z))+
    geom_tile(data=gfdf, aes(x=Longitude,y=Latitude,fill=Hrpkm2))+
    facet_wrap(~gridsize)+
    geom_polygon(data = eez, aes(x = long, y = lat, group = group), colour = "dark grey", fill = NA)+
    geom_sf(color = "grey", fill = "lightgrey")+
    coord_sf(xlim=c(Xlim[1]-.5,Xlim[2]+.5),ylim=c(Ylim[1]-.5,Ylim[2]+.5), expand = FALSE)+
    geom_contour(data=bathy,aes(x=x,y=y,z=z),
                 breaks=c(-50),
                 colour="dark grey", size=0.5  )+
    # geom_polygon(data = jra, aes(x = long, y = lat, group = group), colour = "orange", fill = NA)+
    # scale_fill_continuous(type='viridis',na.value='white',limits=Zlim)
  
  geom_polygon(data = jra, aes(x = long, y = lat, group = group), colour = "orange", fill = NA)+
    scale_fill_continuous(type='viridis',na.value='white',limits=Zlim)
  
  
  if(create_plot==T){
    print(g)}
  if(ggplot_object==T){return(g)}else{return(gfdf)}

}