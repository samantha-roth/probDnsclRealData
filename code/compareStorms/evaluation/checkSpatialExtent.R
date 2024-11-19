
rm(list=ls())

library(terra)

setwd("C:/Users/svr5482/Documents/ArcGIS/Projects/probDnsclMakeExtentsMatch")

dem5m<- rast("norristown_5m.tif")
run2014<- rast("Run_2014.tif")
run2020<- rast("Run_2020.tif")
runfuture<- rast("Run_future.tif")


WSE2014<- rast("WSE2014.tif")
WSE2020<- rast("WSE2020.tif")
WSEfuture<- rast("WSEfuture.tif")

ext(run2020)
ext(WSE2020)
ext(dem5m)

plot(WSE2020)
plot(dem5m)

coords10m<- xyFromCell(WSE2020,1:ncell(WSE2020))
length(unique(coords10m[,1]))
length(unique(coords10m[,2]))

coords5m<- xyFromCell(dem5m,1:ncell(dem5m))
length(unique(coords5m[,1]))
length(unique(coords5m[,2]))