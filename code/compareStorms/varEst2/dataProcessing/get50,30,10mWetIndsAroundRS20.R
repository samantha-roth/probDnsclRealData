rm(list=ls())

library(terra)

setwd("C:/Users/svr5482")

load("probabilisticDownscaling/data/simObs/boxAroundRS20.5m.RData")

#load the high water marks
load("probabilisticDownscaling/data/simObs/obsWE_RS20.RData")

flow<- c("Q2559.8429","Q2503.2092","Q3681.19006")

for(f in 1:length(flow)){
  if(dir.exists(paste0("probabilisticDownscaling/data/",flow[f]))==F){
    dir.create(paste0("probabilisticDownscaling/data/",flow[f]))}
  
  if(dir.exists(paste0("probabilisticDownscaling/data/",flow[f],"/simObs"))==F){
    dir.create(paste0("probabilisticDownscaling/data/",flow[f],"/simObs"))}
  
  #load the calibrated projections at each resolution
  setwd("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha")
  run50m<- rast(paste0("Outputs50m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  run30m<- rast(paste0("Outputs30m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  run10m<- rast(paste0("Outputs10m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  run5m<- rast(paste0("Outputs5m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  
  setwd("C:/Users/svr5482")

  #get the lower resolution projections at the right locations
  vals50minBds<- c(as.matrix(extract(run50m,coordsinBds.5m)))
  vals30minBds<- c(as.matrix(extract(run30m,coordsinBds.5m)))
  vals10minBds<- c(as.matrix(extract(run10m,coordsinBds.5m)))
  vals5minBds<- c(as.matrix(extract(run5m,coordsinBds.5m)))
  
  floodInds50mat5m<- which(vals50minBds>0)
  floodInds30mat5m<- which(vals30minBds>0)
  floodInds10mat5m<- which(vals10minBds>0)
  
  save(floodInds50mat5m,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/floodInds50mat5mAroundRS20.RData"))
  save(floodInds30mat5m,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/floodInds30mat5mAroundRS20.RData"))
  save(floodInds10mat5m,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/floodInds10mat5mAroundRS20.RData"))
  
  
  destInds10m<- good5minds[-floodInds10mat5m]
  destIndsinGoodInds10m<- which(good5minds%in%destInds10m)
  
  save(destIndsinGoodInds10m, destInds10m,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/destInds10mat5m.RData"))
  
  
  destInds30m<- good5minds[-floodInds30mat5m]
  destIndsinGoodInds30m<- which(good5minds%in%destInds30m)
  
  save(destIndsinGoodInds30m, destInds30m,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/destInds30mat5m.RData"))
  
  destInds50m<- good5minds[-floodInds50mat5m]
  destIndsinGoodInds50m<- which(good5minds%in%destInds50m)
  
  save(destIndsinGoodInds50m, destInds50m,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/destInds50mat5m.RData"))
  
}
