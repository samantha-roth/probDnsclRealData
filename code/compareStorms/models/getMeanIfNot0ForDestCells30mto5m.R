#get the distribution for the high resolution cells outside the low res wet cells
#downscale30m -> sourceIndsUnique -> sourceIndsForDests30m -> destInds30m
#mean -> each unique location index-> index of this location repeated at the index of each
#dest cell it reaches -> index of destination cell

rm(list=ls())
library(terra)

pt<-proc.time()

setwd("C:/Users/svr5482")

flow<- c("Q2559.8429","Q2503.2092","Q3681.19006")

for(f in 1:length(flow)){
  run5m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs5m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  coords.5m<- xyFromCell(run5m,1:ncell(run5m))
  inds.5m<- 1:ncell(run5m)
  
  #load cells of interest outside the low res flooded cells
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/destInds30mat5m.RData")) #destInds30m
  
  ##load source cells within the low res flood zone corresponding to the cells of interest
  #load("probabilisticDownscaling/data/simObs/sourcesForDests30m.RData") #sourcesforDest
  
  #get coords of these source cells
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/sourceIndsCoordsForDests30mto5m.RData"))
  
  ################################################################################
  
  #if using downscaled water surface elevation values
  
  #load the downscaled values at the source cells
  load(paste0("probabilisticDownscaling/data/",flow[f],"/WSE.dnsclAtSource30mto5m.RData"))
  
  
  #source mean (given dest not in point mass at zero) to dest
  WSEFromSourceToDest<- rep(NA, length(destInds30m))
  for(i in 1:length(WSE.dnscl)){
    indsMapTo<- which(sourceIndsForDests30m==sourceIndsUnique[i])
    WSEFromSourceToDest[indsMapTo]<- WSE.dnscl[i]
  }
  #save(WSEFromSourceToDest,file="probabilisticDownscaling/data/simObs/WSEdnsclFromSourceToDest30m.RData")
  
  
  #load the elevations at 5m 
  dem5m<- rast("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/LISFLOOD/norristown_5m.asc")
  coords.5mDest<- coords.5m[destInds30m,]
  destElevs<- c(as.matrix(extract(dem5m,coords.5mDest)))
  
  meanFromSourceToDest<- WSEFromSourceToDest-destElevs
  meanFromSourceToDest[which(meanFromSourceToDest<0)]<- 0
  
  save(meanFromSourceToDest,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/shiftbyelevdnsclFromSourceToDest30mto5m.RData"))
  
  ################################################################################
  ##If using the unshifted downscaled values
  
  ##load the downscaled values at the source cells
  #load("probabilisticDownscaling/data/simObs/downscale30mto5mAtSource.RData")
  
  ##load the estimated variance from comparing the downscaled projs to the HWMs
  #load("probabilisticDownscaling/data/simObs/varResHWM30mto5m.RData")
  
  ##source mean (given dest not in point mass at zero) to dest
  #meanFromSourceToDest<- rep(NA, length(destInds30m))
  #for(i in 1:length(downscale30m)){
  #  indsMapTo<- which(sourceIndsForDests30m==sourceIndsUnique[i])
  #  meanFromSourceToDest[indsMapTo]<- downscale30m[i]
  #}
  #save(meanFromSourceToDest,file="probabilisticDownscaling/data/simObs/dnsclFromSourceToDest30mto5m.RData")
  
  
  ptFinal<-proc.time()-pt
  time_meanIfNot0<-ptFinal[3]
  save(time_meanIfNot0, file= paste0("C:/Users/svr5482/probabilisticDownscaling/data/",flow[f],"/time_meanIfNot0_30mto5m.RData"))
  
  ################################################################################
  ##If using the shifted downscaled values
  
  ##load the shifted downscaled values at the source cells
  #load("probabilisticDownscaling/data/simObs/shiftdnscl30mAtSource.RData")
  
  ##load the estimated variance from comparing the downscaled projs to the HWMs
  #load("probabilisticDownscaling/data/simObs/varShiftResHWM30m.RData")
  
  ################################################################################
  ##source mean (given dest!=0) to dest
  #meanFromSourceToDest<- rep(NA, length(destInds30m))
  #for(i in 1:length(shiftdnscl30m)){
  #  indsMapTo<- which(sourceIndsForDests30m==sourceIndsUnique[i])
  #  meanFromSourceToDest[indsMapTo]<- shiftdnscl30m[i]
  #}
  
  #save(meanFromSourceToDest,file="probabilisticDownscaling/data/simObs/shiftdnsclFromSourceToDest30m.RData")
  ################################################################################
  
}

