rm(list=ls())

library(terra)

setwd("C:/Users/svr5482/Documents/GitHub/probDnsclRealData")

#load the calibrated projections at each resolution
run10m2014<- rast(paste0("data/Outputs10m/flood2014/Run_1.asc"))
run10m2020<- rast(paste0("data/Outputs10m/flood2020/Run_1.asc"))
run10mfuture<- rast(paste0("data/Outputs10m/floodfuture/Run_1.asc"))

#load the DEM files at each resolution
dem5m<- rast("data/norristown_5m.asc")
dem10m<- rast("data/norristown_10m_new.asc")

crs(dem5m)
crs(dem10m)
crs(run10m2014)
crs(run10m2020)
crs(run10mfuture)

load("C:/Users/svr5482/probDnsclRealData/data/WGS84")

crs(dem5m)<- WGS84
crs(dem10m)<- WGS84
crs(run10m2014)<- WGS84
crs(run10m2020)<- WGS84
crs(run10mfuture)<- WGS84

writeRaster(run10m2014,file=paste0("data/Outputs10m/flood2014/Run_1.tif"),overwrite=TRUE)
writeRaster(run10m2020,file=paste0("data/Outputs10m/flood2020/Run_1.tif"),overwrite=TRUE)
writeRaster(run10mfuture,file=paste0("data/Outputs10m/floodfuture/Run_1.tif"),overwrite=TRUE)
writeRaster(dem5m,file="data/norristown_5m.tif")
writeRaster(dem10m,file="data/norristown_10m_new.tif")

plot(dem5m)
plot(dem10m)
plot(run10m2014)
plot(run10m2020)
plot(run10mfuture)