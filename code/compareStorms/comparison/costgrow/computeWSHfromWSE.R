#subtract high resolution elevations from high resolution WSEs


rm(list=ls())

library(terra)

ndArgs(trailingOnly=TRUE)
setwd(dir)

run5m<- rast("data/Outputs5m/Run_1.asc")
dem5m<- rast("data/norristown_5m.asc")
load("data/coords.5m.RData")
elev5m<- c(as.matrix(extract(dem5m,coords.5m)))

flood<- c("flood2014","flood2020","floodfuture")

for(f in 1:length(flood)){
  
  run<- rast(paste0("data/",flood[f],"/CG_",flood[f],".tif"))
  WSE<- c(as.matrix(extract(run,coords.5m)))
  WSH<- WSE- elev5m
  WSH[which(WSH<0)]<-NA
  WSHrast<- run5m
  values(WSHrast)<- WSH
  print(plot(WSHrast))
  
  writeRaster(WSHrast,file=paste0("data/",flood[f],"/CG_WSH.tif"),overwrite=TRUE)
}


