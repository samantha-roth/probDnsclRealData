rm(list=ls())

library(terra)

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

#load the high water marks
load("data/HWMsdf.RData")
HWMlocs<- as.matrix(HWMs.df[,c("x","y")]); HWMlocs<- HWMlocs[1:5,]

#load the calibrated projections at each resolution
run10m<- rast("data/Outputs10m/Run_1.asc")
run5m<- rast("data/Outputs5m/Run_1.asc")

#load the DEM files at each resolution
dem5m<- rast("data/norristown_5m.asc")
dem10m<- rast("data/norristown_10m_new.asc")

################################################################################
#get 10m flood predictions in a box around the high water marks

load("data/boxAroundHWMs.5m.RData")

#get the lower resolution projections at the right locations
vals10minBds<- c(as.matrix(extract(run10m,coordsinBds.5m)))
vals5minBds<- c(as.matrix(extract(run5m,coordsinBds.5m)))

#get lower resolution elevations at the right locations
elev10minBds<- c(as.matrix(extract(dem10m,coordsinBds.5m)))
elev5minBds<- c(as.matrix(extract(dem5m,coordsinBds.5m)))

#adjust the values using the 5m DEM
vals10minBdsAdj<- vals10minBds+elev10minBds-elev5minBds
vals10minBdsAdj[which(vals10minBdsAdj<0)]<- 0

save(vals10minBdsAdj,file="data/vals10mto5minBdsAroundHWMsAdj.RData")
save(vals5minBds,file="data/vals5minBdsAroundHWMs.RData")

################################################################################
################################################################################
#get adjusted 10m flood predictions at the 20 high water marks

vals10m<- c(as.matrix(extract(run10m,HWMlocs)))
vals5m<- c(as.matrix(extract(run5m,HWMlocs)))

elev10m<- c(as.matrix(extract(dem10m,HWMlocs)))
elev5m<- c(as.matrix(extract(dem5m,HWMlocs)))

#adjust the values using the 5m DEM
vals10mAdj<- vals10m+elev10m-elev5m
vals10mAdj[which(vals10mAdj<0)]<- 0

save(vals10mAdj,file="data/HWMlocsvals10mto5mAtHWMsAdj.RData")
save(vals5m,file="data/HWMlocsvals5mAtHWMs.RData")