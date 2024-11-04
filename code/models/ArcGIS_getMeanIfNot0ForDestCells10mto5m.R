#get the distribution for the high resolution cells outside the low res wet cells
#downscale10m -> sourceIndsUnique -> sourceIndsForDests10m -> destInds10m
#mean -> each unique location index-> index of this location repeated at the index of each
#dest cell it reaches -> index of destination cell

rm(list=ls())
library(terra)

pt<-proc.time()

setwd("C:/Users/svr5482")

run5m<- rast("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs5m/Norristown/nCh/PostMedCh/Extent/Run_1.asc")
load("probDnsclRealData/data/coords.5m.RData")
inds.5m<- 1:ncell(run5m)

#load cells of interest outside the low res flooded cells
load("probDnsclRealData/data/destInds10mat5m.RData") #destInds10m

##load source cells within the low res flood zone corresponding to the cells of interest
#load("probDnsclRealData/data/sourcesForDests10m.RData") #sourcesforDest

#get coords of these source cells
load("probDnsclRealData/data/sourceIndsCoordsForDests10mto5m.RData")

################################################################################

#if using downscaled water surface elevation values

#load the downscaled values at the source cells
load("probDnsclRealData/data/WSE.dnsclAtSource10mto5m.RData")


#source mean (given dest not in point mass at zero) to dest
WSEFromSourceToDest<- rep(NA, length(destInds10m))
for(i in 1:length(WSE.dnscl)){
  indsMapTo<- which(sourceIndsForDests10m==sourceIndsUnique[i])
  WSEFromSourceToDest[indsMapTo]<- WSE.dnscl[i]
}
#save(WSEFromSourceToDest,file="probDnsclRealData/data/WSEdnsclFromSourceToDest10m.RData")


#load the elevations at 5m 
dem5m<- rast("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/LISFLOOD/norristown_5m.asc")
coords.5mDest<- coords.5m[destInds10m,]
destElevs<- c(as.matrix(extract(dem5m,coords.5mDest)))

meanFromSourceToDest<- WSEFromSourceToDest-destElevs
meanFromSourceToDest[which(meanFromSourceToDest<0)]<- 0

save(meanFromSourceToDest,file="probDnsclRealData/data/shiftbyelevdnsclFromSourceToDest10mto5m.RData")

################################################################################
##If using the unshifted downscaled values

##load the downscaled values at the source cells
#load("probDnsclRealData/data/downscale10mto5mAtSource.RData")

##load the estimated variance from comparing the downscaled projs to the HWMs
#load("probDnsclRealData/data/varResHWM10m.RData")

##source mean (given dest not in point mass at zero) to dest
#meanFromSourceToDest<- rep(NA, length(destInds10m))
#for(i in 1:length(downscale10m)){
#  indsMapTo<- which(sourceIndsForDests10m==sourceIndsUnique[i])
#  meanFromSourceToDest[indsMapTo]<- downscale10m[i]
#}
#save(meanFromSourceToDest,file="probDnsclRealData/data/dnsclFromSourceToDest10mat5m.RData")


ptFinal<-proc.time()-pt
time_meanIfNot0<-ptFinal[3]
save(time_meanIfNot0, file= "C:/Users/svr5482/probDnsclRealData/data/time_meanIfNot0_10mto5m.RData")

################################################################################
##If using the shifted downscaled values

##load the shifted downscaled values at the source cells
#load("probDnsclRealData/data/shiftdnscl10mAtSource.RData")

##load the estimated variance from comparing the downscaled projs to the HWMs
#load("probDnsclRealData/data/varShiftResHWM10m.RData")

################################################################################
##source mean (given dest!=0) to dest
#meanFromSourceToDest<- rep(NA, length(destInds10m))
#for(i in 1:length(shiftdnscl10m)){
#  indsMapTo<- which(sourceIndsForDests10m==sourceIndsUnique[i])
#  meanFromSourceToDest[indsMapTo]<- shiftdnscl10m[i]
#}

#save(meanFromSourceToDest,file="probDnsclRealData/data/shiftdnsclFromSourceToDest10m.RData")
################################################################################
