# AOI.R
# Anne-Elise Nieblas
# 12/11/2019
# Description: Selects points within an area of interest.

aoi<-function(dataset,shp,create_plot=FALSE,remove_points=FALSE,shp_type=NA){
  #' @name aoi
 
  # library(sp)
  # library(maptools)
  # library(rgdal)
  
  pp <- readOGR(dsn = shp, stringsAsFactors = F)
  
  
  # data(wrld_simpl)
  # data(wrldHiRes)
  
  ## Create a SpatialPoints object
  set.seed(0)
  points<-data.frame(Longitude=as.numeric(dataset$Longitude),Latitude=as.numeric(dataset$Latitude))
  dataset<-dataset[complete.cases(points),]
  points<-points[complete.cases(points),]
  pts <- SpatialPoints(points, proj4string=CRS(proj4string(pp)))
  
  dataset$Latitude<-as.numeric(dataset$Latitude)
  dataset$Longitude<-as.numeric(dataset$Longitude)
  
  # convert to spdf and over function
  coordinates(dataset)<-~Longitude+Latitude
  wgs<-proj4string(pp) 
  # proj4string(pp) <- CRS(wgs)
  pp.polygons <- spTransform(pp, CRS("+proj=longlat"))
  proj4string(dataset) <- CRS(proj4string(pp))
  
  # now do OVER and bind the data to the dataframe
  io<-over(dataset,pp)
  dataset@data = cbind(dataset@data,io) 
  # summary(dataset)### SEE THE RESULTS FROM SUMMARY AND COLUMN TYPE
  if(shp_type=='eez'){
  datos_m<-subset(dataset,!is.na(dataset$geoname))}
  if(shp_type=='rfb'){
    datos_m<-subset(dataset,!is.na(dataset$RFB))    
  }
  
  
  datos_m$lat<-datos_m@coords[,2]
  datos_m$lon<-datos_m@coords[,1]
  
  if(create_plot==TRUE){
    # if(save_plot==TRUE){jpeg()}
  plot(pp,col='blue')
  points(points$Longitude, points$Latitude,col='black')
  points(datos_m$lon,datos_m$lat,col='red',pch=16)
  }
  
  if(remove_points==TRUE){
  return(datos_m)  
    }else{return(dataset)}
  
  }
  
  
  
  
