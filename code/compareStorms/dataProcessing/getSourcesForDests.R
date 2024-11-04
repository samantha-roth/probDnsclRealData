#get the coordinates of the high resolution cells inside the low res wet cells
#that are sources for the destination 5m cells (outside the low res wet cells)
#in the zone of interest

rm(list=ls())
library(terra)

pt<-proc.time()

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

flood<- c("flood2014","flood2020","floodfuture")

for(f in 1:length(flood)){
  run5m<- rast(paste0("data/Outputs5m/",flood[f],"/Run_1.asc"))
  coords.5m<- xyFromCell(run5m,1:ncell(run5m))
  inds.5m<- 1:ncell(run5m)
  
  #load cells of interest outside the low res flooded cells
  load(paste0("data/",flood[f],"/destInds10mat5m.RData"))
  #load source cells within the low res flood zone corresponding to the cells of interest
  load(paste0("data/",flood[f],"/sourcesForDests10mto5m_QGIS.RData"))
  
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
       file=paste0("data/",flood[f],"/sourceIndsCoordsForDests10mto5m_QGIS.RData"))
  
  ptFinal<-proc.time()-pt
  time_sourcesForDests<-ptFinal[3]
  save(time_sourcesForDests, file= paste0("data/",flood[f],"/time_sourcesForDests10mto5m_QGIS.RData"))
  
}
