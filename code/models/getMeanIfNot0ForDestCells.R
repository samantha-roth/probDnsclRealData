#get the distribution for the high resolution cells outside the low res wet cells
#downscale10m -> sourceIndsUnique -> sourceIndsForDests10m -> destInds10m
#mean -> each unique location index-> index of this location repeated at the index of each
#dest cell it reaches -> index of destination cell

rm(list=ls())
library(terra)

pt<-proc.time()

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

run5m<- rast("data/Outputs5m/Run_1.asc")
load("data/coords.5m.RData")
inds.5m<- 1:ncell(run5m)

#load cells of interest outside the low res flooded cells
load("data/destInds10mat5m.RData") #destInds10m

#get coords of these source cells
load("data/sourceIndsCoordsForDests10mto5m_QGIS.RData")

################################################################################

#using downscaled water surface elevation values

#load the downscaled values at the source cells
load("data/WSE.dnsclAtSource10mto5m_QGIS.RData")


#source mean (given dest not in point mass at zero) to dest
WSEFromSourceToDest<- rep(NA, length(destInds10m))
for(i in 1:length(WSE.dnscl)){
  indsMapTo<- which(sourceIndsForDests10m==sourceIndsUnique[i])
  WSEFromSourceToDest[indsMapTo]<- WSE.dnscl[i]
}
#save(WSEFromSourceToDest,file="data/WSEdnsclFromSourceToDest10m.RData")


#load the elevations at 5m 
dem5m<- rast("data/norristown_5m.asc")
coords.5mDest<- coords.5m[destInds10m,]
destElevs<- c(as.matrix(extract(dem5m,coords.5mDest)))

meanFromSourceToDest<- WSEFromSourceToDest-destElevs
meanFromSourceToDest[which(meanFromSourceToDest<0)]<- 0

save(meanFromSourceToDest,file="data/shiftbyelevdnsclFromSourceToDest10mto5m_QGIS.RData")

ptFinal<-proc.time()-pt
time_meanIfNot0<-ptFinal[3]
save(time_meanIfNot0, file= "data/time_meanIfNot0_10mto5m_QGIS.RData")
