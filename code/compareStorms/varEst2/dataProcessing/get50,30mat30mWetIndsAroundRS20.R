rm(list=ls())

library(terra)

setwd("C:/Users/svr5482")

#load the high water marks
load("probabilisticDownscaling/data/simObs/obsWE_RS20.RData")

load("probabilisticDownscaling/data/simObs/boxAroundRS20.30m.RData")

flow<- c("Q2559.8429","Q2503.2092","Q3681.19006")

for(f in 1:length(flow)){
  
  #load the calibrated projections at each resolution
  run50m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs50m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  run30m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs30m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  
  #get the lower resolution projections at the right locations
  vals50minBds<- c(as.matrix(extract(run50m,coordsinBds.30m)))
  vals30minBds<- c(as.matrix(extract(run30m,coordsinBds.30m)))
  
  floodInds50mat30m<- which(vals50minBds>0)
  floodInds30mat30m<- which(vals30minBds>0)
  
  save(floodInds50mat30m,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/floodInds50mat30mAroundRS20.RData"))
  save(floodInds30mat30m,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/floodInds30mat30mAroundRS20.RData"))
  
  
  destInds30m<- good30minds[-floodInds30mat30m]
  destIndsinGoodInds30m<- which(good30minds%in%destInds30m)
  
  save(destIndsinGoodInds30m, destInds30m,
       file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/destInds30mat30m.RData"))
  
  destInds50m<- good30minds[-floodInds50mat30m]
  destIndsinGoodInds50m<- which(good30minds%in%destInds50m)
  
  save(destIndsinGoodInds50m, destInds50m,
       file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/destInds50mat30m.RData"))
  
}
