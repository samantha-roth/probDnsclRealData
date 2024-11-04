rm(list=ls())

library(terra)

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

load("data/boxAroundHWMs.5m.RData")

#load the high water marks
load("data/HWMsdf.RData")
HWMlocs<- as.matrix(HWMs.df[,c("x","y")]); HWMlocs<- HWMlocs[1:5,]

flood<- c("flood2014","flood2020","floodfuture")

for(f in 1:length(flood)){
  if(dir.exists(paste0("data/",flood[f]))==F){
    dir.create(paste0("data/",flood[f]))}
  
  if(dir.exists(paste0("data/",flood[f],"/simObs"))==F){
    dir.create(paste0("data/",flood[f],"/simObs"))}
  
  #load the calibrated projections at each resolution
  run10m<- rast(paste0("data/Outputs10m/",flood[f],"/Run_1.asc"))
  run5m<- rast(paste0("data/Outputs5m/",flood[f],"/Run_1.asc"))

  vals10minBds<- c(as.matrix(extract(run10m,coordsinBds.5m)))
  vals5minBds<- c(as.matrix(extract(run5m,coordsinBds.5m)))
  
  floodInds10mat5m<- which(vals10minBds>0)
  
  save(floodInds10mat5m,file=paste0("data/",flood[f],"/floodInds10mat5mAroundHWMs.RData"))
  
  
  destInds10m<- good5minds[-floodInds10mat5m]
  destIndsinGoodInds10m<- which(good5minds%in%destInds10m)
  
  save(destIndsinGoodInds10m, destInds10m,file=paste0("data/",flood[f],"/destInds10mat5m.RData"))
  
}
