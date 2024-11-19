#compute WSE

rm(list=ls())

library(terra)

setwd("C:/Users/svr5482/Documents/GitHub/probDnsclRealData")

run2014<- rast(paste0("data/Outputs10m/flood2014/Run_1.tif"))
run2020<- rast(paste0("data/Outputs10m/flood2020/Run_1.tif"))
runfuture<- rast(paste0("data/Outputs10m/floodfuture/Run_1.tif"))
dem5m<- rast("data/norristown_5m.tif")
dem10m<- rast("data/norristown_10m_new.tif")

WSE2014<- run2014
values(WSE2014)<- values(run2014)+values(dem10m)

WSE2020<- run2020
values(WSE2020)<- values(run2020)+values(dem10m)

WSEfuture<- runfuture
values(WSEfuture)<- values(runfuture)+values(dem10m)

writeRaster(WSE2014,file=paste0("data/Outputs10m/flood2014/WSE.tif"),overwrite=TRUE)
writeRaster(WSE2020,file=paste0("data/Outputs10m/flood2020/WSE.tif"),overwrite=TRUE)
writeRaster(WSEfuture,file=paste0("data/Outputs10m/floodfuture/WSE.tif"),overwrite=TRUE)