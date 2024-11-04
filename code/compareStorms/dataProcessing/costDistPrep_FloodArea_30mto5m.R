#translate cells to flooded or not

rm(list=ls())

setwd("C:/Users/svr5482")

library(terra)

#load high water marks
load("probabilisticDownscaling/data/simObs/obsWE_RS20.RData")

pt<-proc.time()

#load 5m coords
load("C:/Users/svr5482/probabilisticDownscaling/data/coords.5m.RData")

flow<- c("Q2559.8429","Q2503.2092","Q3681.19006")

for(f in 1:length(flow)){
  
  #load calibrated projections at each resolution
  run30m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs30m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  run5m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs5m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  
  ################################################################################
  #make raster files that have value 1 at locations that are flooded 
  #and value NA at locations that are not flooded
  
  vals30m<- c(as.matrix(extract(run30m,coords.5m)))
  vals5m<- c(as.matrix(extract(run5m,coords.5m)))
  
  vals30mBin<- ifelse(vals30m>0,1,NA)
  vals5mBin<- ifelse(vals5m>0,1,NA)
  
  bin30m<- run5m
  values(bin30m)<- vals30mBin
  
  bin5m<- run5m
  values(bin5m)<- vals5mBin
  
  #all1.5m<- run5m
  #values(all1.5m)<- 1
  #plot(all1.5m)
  
  #writeRaster(all1.5m,file="C:/Users/svr5482/probabilisticDownscaling/data/",flow[f],"/all1.5m.tif",overwrite=TRUE)
  writeRaster(bin30m,file=paste0("C:/Users/svr5482/probabilisticDownscaling/data/",flow[f],"/bin30mat5m.tif"),overwrite=TRUE)
  #writeRaster(bin5m,paste0("C:/Users/svr5482/probabilisticDownscaling/data/",flow[f],"/bin5m.tif"),overwrite=TRUE)
  
  #writeRaster(all1.5m,file="C:/Users/svr5482/probabilisticDownscaling/data/all1.5m.asc",overwrite=TRUE)
  writeRaster(bin30m,file=paste0("C:/Users/svr5482/probabilisticDownscaling/data/",flow[f],"/bin30mat5m.asc"),overwrite=TRUE)
  writeRaster(bin5m,paste0("C:/Users/svr5482/probabilisticDownscaling/data/",flow[f],"/bin5m.asc"),overwrite=TRUE)
  
  #bin30mat5m<- rast("C:/Users/svr5482/probabilisticDownscaling/data/bin30mat5m.tif")
  #coordsbin30mat5m<- xyFromCell(bin30mat5m,1:ncell(bin30mat5m))
  
  ptFinal<-proc.time()-pt
  time_costDistPrep_FloodArea<-ptFinal[3]
  save(time_costDistPrep_FloodArea, file= paste0("C:/Users/svr5482/probabilisticDownscaling/data/",flow[f],"/time_costDistPrep_FloodArea_30mto5m.RData"))
  
}

