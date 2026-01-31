#get the coordinates of interest at the 10m resolution

rm(list=ls())

# dir<- commandArgs(trailingOnly=TRUE)
# setwd(dir)

setwd("/Users/f007f8t/Documents/GitHub/probDnsclRealData")

library(terra)

#load the high water marks
load("data/HWMsdf.RData")
HWMlocs<- as.matrix(HWMs.df[,c("x","y")]); HWMlocs<- HWMlocs[1:5,]

#load the calibrated projections at each resolution
run10m<- rast("data/Outputs10m/Run_1.asc")
load("data/coords.10m.RData")

vals10m<- c(as.matrix(extract(run10m,HWMlocs)))

minx_obs<- min(HWMlocs[,1]); miny_obs<- min(HWMlocs[,2])
maxx_obs<- max(HWMlocs[,1]); maxy_obs<- max(HWMlocs[,2])

goodx10m<- which(coords.10m[,1]<=round(maxx_obs,-2)+6)
good10minds<- goodx10m
coordsinBds.10m<- coords.10m[goodx10m,]

save(coordsinBds.10m,good10minds,file="data/boxAroundHWMs.10m.RData")
