#get the coordinates of the high resolution cells inside the low res wet cells
#that are sources for the destination 5m cells (outside the low res wet cells)
#in the zone of interest

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
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/destInds10mat5m.RData"))
  #load source cells within the low res flood zone corresponding to the cells of interest
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/sourcesForDests10mto5m.RData"))
  
  ################################################################################
  #inds of 5m locations we want to get distributions for
  indsOfInterest<- inds.5m[destInds10m]
  
  uX<- unique(coords.5m[,1])
  uY<- unique(coords.5m[,2])
  
  uXord<- order(unique(coords.5m[,1]))
  uYord<- order(unique(coords.5m[,2]))
  
  ords.5m<- matrix(NA,nrow=nrow(coords.5m),ncol=2)
  
  for(i in 1:length(uX)){
    x.inds<- which(coords.5m[,1]==uX[i])
    ords.5m[x.inds,1]<- uXord[i]
  }
  
  for(i in 1:length(uY)){
    y.inds<- which(coords.5m[,2]==uY[i])
    ords.5m[y.inds,2]<- uYord[i]
  }
  
  sourceIndsForDests10m<- rep(NA,nrow(sourcesForDests10m))
  for(i in 1:nrow(sourcesForDests10m)){
    ind<- which(ords.5m[,1]==sourcesForDests10m[i,1] & ords.5m[,2]==sourcesForDests10m[i,2])
    sourceIndsForDests10m[i]<- ind
  }
  
  sourceCoordsForDests10m<- coords.5m[sourceIndsForDests10m,]
  sourceCoordsUnique<- coords.5m[unique(sourceIndsForDests10m),]
  sourceIndsUnique<- unique(sourceIndsForDests10m)
  save(sourceCoordsForDests10m,sourceIndsForDests10m,
       sourceCoordsUnique, sourceIndsUnique,
       file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/sourceIndsCoordsForDests10mto5m.RData"))
  
  ptFinal<-proc.time()-pt
  time_sourcesForDests<-ptFinal[3]
  save(time_sourcesForDests, file= paste0("C:/Users/svr5482/probabilisticDownscaling/data/",flow[f],"/time_sourcesForDests10mto5m.RData"))
  
}
