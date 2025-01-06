#translate cells to flooded or not

rm(list=ls())

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

library(terra)

pt<-proc.time()

#load 5m coords
load("data/coords.5m.RData")

flood<- c("flood2014","flood2020","floodfuture")

for(f in 1:length(flood)){
  
  #load calibrated projections at each resolution
  run10m<- rast(paste0("data/Outputs10m/",flood[f],"/Run_1.asc"))
  run5m<- rast(paste0("data/Outputs5m/",flood[f],"/Run_1.asc"))
  
  ################################################################################
  #make raster files that have value 1 at locations that are flooded 
  #and value NA at locations that are not flooded
  
  vals10m<- c(as.matrix(extract(run10m,coords.5m)))
  vals5m<- c(as.matrix(extract(run5m,coords.5m)))
  
  vals10mBin<- ifelse(vals10m>0,1,NA)
  vals5mBin<- ifelse(vals5m>0,1,NA)
  
  bin10m<- run5m
  values(bin10m)<- vals10mBin
  
  bin5m<- run5m
  values(bin5m)<- vals5mBin
  
  crs(bin10m)<- "+proj=utm +zone=18 +datum=WGS84  +units=m"
  crs(bin5m)<- "+proj=utm +zone=18 +datum=WGS84  +units=m"
  
  writeRaster(bin10m,file=paste0("data/",flood[f],"/bin10mat5m.tif"),overwrite=TRUE)
  writeRaster(bin5m,paste0("data/",flood[f],"/bin5m.tif"),overwrite=TRUE)
  
  ptFinal<-proc.time()-pt
  time_costDistPrep_FloodArea<-ptFinal[3]
  save(time_costDistPrep_FloodArea, file= paste0("data/",flood[f],"/time_costDistPrep_FloodArea_10mto5m.RData"))
  
}
