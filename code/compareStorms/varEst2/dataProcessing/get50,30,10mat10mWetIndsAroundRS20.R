rm(list=ls())

library(terra)

setwd("C:/Users/svr5482")

#load the high water marks
load("probabilisticDownscaling/data/simObs/obsWE_RS20.RData")

flow<- c("Q2559.8429","Q2503.2092","Q3681.19006")

for(f in 1:length(flow)){

  #load the calibrated projections at each resolution
  run50m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs50m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  run30m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs30m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  run10m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs10m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  
  load("probabilisticDownscaling/data/simObs/boxAroundRS20.10m.RData")
  
  #get the lower resolution projections at the right locations
  vals50minBds<- c(as.matrix(extract(run50m,coordsinBds.10m)))
  vals30minBds<- c(as.matrix(extract(run30m,coordsinBds.10m)))
  vals10minBds<- c(as.matrix(extract(run10m,coordsinBds.10m)))
  
  floodInds50mat10m<- which(vals50minBds>0)
  floodInds30mat10m<- which(vals30minBds>0)
  floodInds10mat10m<- which(vals10minBds>0)
  
  save(floodInds50mat10m,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/floodInds50mat10mAroundRS20.RData"))
  save(floodInds30mat10m,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/floodInds30mat10mAroundRS20.RData"))
  save(floodInds10mat10m,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/floodInds10mat10mAroundRS20.RData"))
  
  
  destInds10m<- good10minds[-floodInds10mat10m]
  destIndsinGoodInds10m<- which(good10minds%in%destInds10m)
  
  save(destIndsinGoodInds10m, destInds10m,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/destInds10mat10m.RData"))
  
  destInds50m<- good10minds[-floodInds50mat10m]
  destIndsinGoodInds50m<- which(good10minds%in%destInds50m)
  
  save(destIndsinGoodInds50m, destInds50m,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/destInds50mat10m.RData"))
  
  destInds30m<- good10minds[-floodInds30mat10m]
  destIndsinGoodInds30m<- which(good10minds%in%destInds30m)
  
  save(destIndsinGoodInds30m, destInds30m,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/destInds30mat10m.RData"))
  
  
}

