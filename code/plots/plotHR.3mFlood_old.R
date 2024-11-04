#get sensitivity and specificity in wet low resolution cell area

rm(list=ls())

setwd("C:/Users/svr5482")

library(terra); library(RColorBrewer)

run5m<- rast("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs5m/Norristown/nCh/PostMedCh/Extent/Run_1.asc")
coords.5m<- xyFromCell(run5m,1:ncell(run5m))

################################################################################
#load estimated variance
load("probDnsclRealData/data/varResHWM10mto5m.RData")

#load flooded locations at the two lower resolutions being downscaled
load("probDnsclRealData/data/floodInds10mat5mAroundHWMs.RData")

#load region of interest coordinates
load("probDnsclRealData/data/boxAroundHWMs.5m.RData")

#load calibrated 5m flood projections in region of interest
load("probDnsclRealData/data/vals5minBdsAroundHWMs.RData")

#load downscaled calibrated flood projections in region of interest
load("probDnsclRealData/data/downscale10mto5mAroundHWMs.RData")

#load cells of interest outside the low res flooded cells
load("probDnsclRealData/data/destInds10mat5m.RData")

length(which(good5minds%in%good5minds[c(destIndsinGoodInds10m,floodInds10mat5m)]))

################################################################################
#get flooded and not flooded locations within the business region of interest
floodInds<- good5minds[floodInds10mat5m]

wsh.5m<- c(as.matrix(extract(run5m,coordsinBds.5m)))
HighResFlood<- ifelse(wsh.5m>0,1,0)
HighResFlood.3m<- ifelse(wsh.5m>0.3,1,0)


#define spatial extent using a bounding box (left, right, bottom, top)
bbox_to_keep <- c(min(coordsinBds.5m[,1])-1, max(coordsinBds.5m[,1])+1, 
                  min(coordsinBds.5m[,2])-1, max(coordsinBds.5m[,2])+1)
extent_to_keep <- ext(bbox_to_keep)

#Crop the raster to the new extent
flood.3m <- crop(run5m, extent_to_keep)

#coords_pFlood.3m<- xyFromCell(pFlood.3m,1:ncell(pFlood.3m))

values(flood.3m)<- HighResFlood.3m

filename<- "probDnsclRealData/plots/HighResFlood.3m_5m.jpeg"
jpeg(file = filename,width = 300,height=640)
plot(flood.3m,axes=FALSE)
dev.off()
