#get the coordinates of interest at the 5m resolution

rm(list=ls())

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

library(terra)

#load the high water marks
load("data/HWMsdf.RData")
HWMlocs<- as.matrix(HWMs.df[,c("x","y")]); HWMlocs<- HWMlocs[1:5,]

#load the calibrated projections at each resolution
run5m<- rast("data/Outputs5m/Run_1.asc")
load("data/coords.5m.RData")

vals5m<- c(as.matrix(extract(run5m,HWMlocs)))

minx_obs<- min(HWMlocs[,1]); miny_obs<- min(HWMlocs[,2])
maxx_obs<- max(HWMlocs[,1]); maxy_obs<- max(HWMlocs[,2])

goodx5m<- which(coords.5m[,1]<=round(maxx_obs,-2))
good5minds<- goodx5m
coordsinBds.5m<- coords.5m[goodx5m,]

save(coordsinBds.5m,good5minds,file="data/boxAroundHWMs.5m.RData")
