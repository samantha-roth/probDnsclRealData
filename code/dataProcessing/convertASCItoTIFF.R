

rm(list=ls())

library(terra)

setwd("C:/Users/svr5482")

dem5m<- rast("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/LISFLOOD/norristown_5m.asc")
WGS84<- crs(dem5m)
writeRaster(dem5m,"FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/LISFLOOD/norristown_5m.tif",overwrite=TRUE)

bin10m<- rast("probDnsclRealData/data/bin10mat5m.tif")
crs(bin10m)

crs(bin10m)<- WGS84
crs(bin10m)
writeRaster(bin10m,file="probDnsclRealData/data/bin10mat5m2.tif",overwrite=TRUE)


flood<- c("flood2014","flood2020","floodfuture")

for(f in 1:length(flood)){
  bin10m<- rast(paste0("C:/Users/svr5482/probDnsclRealData/data/",flood[f],"/bin10mat5m.tif"))
  bin5m<- rast(paste0("C:/Users/svr5482/probDnsclRealData/data/",flood[f],"/bin5m.tif"))
  
  crs(bin10m)<- WGS84
  crs(bin5m)<- WGS84
  
  writeRaster(bin10m,file=paste0("C:/Users/svr5482/probDnsclRealData/data/",flood[f],"/bin10mat5m2.tif"),overwrite=TRUE)
  writeRaster(bin5m,paste0("C:/Users/svr5482/probDnsclRealData/data/",flood[f],"/bin5m2.tif"),overwrite=TRUE)
}