#find the maximum flood height

rm(list=ls())

setwd("C:/Users/svr5482")
library(terra)

#load the high water marks
load("probDnsclRealData/data/HWMsdf.RData")
HWMlocs<- as.matrix(HWMs.df[,c("x","y")]); HWMlocs<- HWMlocs[1:5,]
obs<- HWMs.df$height[1:5]

#load 5m calibrated projection
run5m<- rast("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs5m/Norristown/nCh/PostMedCh/Extent/Run_1.asc")
coords.5m<- xyFromCell(run5m,1:ncell(run5m))
preds5m<- c(as.matrix(extract(run5m,coords.5m)))

#load 10m calibrated projection
run10m<- rast("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs10m/Norristown/nCh/PostMedCh/Extent/Run_1.asc")
coords.10m<- xyFromCell(run10m,1:ncell(run10m))
preds10m<- c(as.matrix(extract(run10m,coords.10m)))

maxFH<- max(c(preds5m,preds10m,obs))

save(maxFH,file="C:/Users/svr5482/probDnsclRealData/data/maxFH.RData")