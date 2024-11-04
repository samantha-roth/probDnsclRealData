rm(list=ls())

library(terra)

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

#load the high water marks
load("data/HWMsdf.RData")
HWMlocs<- as.matrix(HWMs.df[,c("x","y")]); HWMlocs<- HWMlocs[1:5,]

#load the calibrated projections at each resolution
run10m<- rast("data/Outputs10m/Run_1.asc")
run5m<-  rast("data/Outputs5m/Run_1.asc")

load("data/boxAroundHWMs.5m.RData")

#get the lower resolution projections at the right locations
vals10minBds<- c(as.matrix(extract(run10m,coordsinBds.5m)))
vals5minBds<- c(as.matrix(extract(run5m,coordsinBds.5m)))

floodInds10mat5m<- which(vals10minBds>0)

save(floodInds10mat5m,file="data/floodInds10mat5mAroundHWMs.RData")


destInds10m<- good5minds[-floodInds10mat5m]
destIndsinGoodInds10m<- which(good5minds%in%destInds10m)

save(destIndsinGoodInds10m, destInds10m,file="data/destInds10mat5m.RData")

