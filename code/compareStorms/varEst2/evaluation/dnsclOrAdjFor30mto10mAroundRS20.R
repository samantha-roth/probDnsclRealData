
rm(list=ls())

library(terra)

pt<- proc.time()

setwd("C:/Users/svr5482")

#load region of interest coordinates
load("probabilisticDownscaling/data/simObs/boxAroundRS20.10m.RData")

#load the DEM files at each resolution
dem10m<- rast("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/LISFLOOD/norristown_10m_new.asc")
dem30m<- rast("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/LISFLOOD/norristown_30m_new.asc")

flow<- c("Q2559.8429","Q2503.2092","Q3681.19006")

for(f in 1:length(flow)){
  
  #load flooded locations at the two lower resolutions being downscaled
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/floodInds30mat10mAroundRS20.RData"))
  
  #load the calibrated projections at each resolution
  run30m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs30m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  run10m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs10m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  ################################################################################
  #Next we use bilinear interpolation to get the WSH at 30m on 10m grid
  
  coords.30m<- xyFromCell(run30m,1:ncell(run30m))
  
  min.x30<- min(coords.30m[,1])
  min.y30<- min(coords.30m[,2])
  max.x30<- max(coords.30m[,1])
  max.y30<- max(coords.30m[,2])
  
  goodLocs<- coordsinBds.10m[floodInds30mat10m,]
  
  y10m<- unique(goodLocs[,2])
  x10m<- unique(goodLocs[,1])
  y30m<- unique(coords.30m[,2])
  x30m<- unique(coords.30m[,1])
  
  nx10m<- length(x10m)
  ny10m<- length(y10m)
  nx30m<- length(x30m)
  ny30m<- length(y30m)
  
  library(akima)
  
  elev.10m<- c(as.matrix(extract(dem10m,goodLocs)))
  elev.30m<- c(as.matrix(extract(dem30m,coords.30m)))
  wsh.30m<- c(as.matrix(extract(run30m,coords.30m)))
  vals.30m<- wsh.30m + elev.30m
  
  z1= matrix(vals.30m, nrow= ny30m, ncol= nx30m, byrow= TRUE)
  
  z= matrix(NA,nrow= ny30m, ncol= nx30m)
  for(j in 1:nx30m){ z[,j]<- rev(z1[,j]) }
  
  test<- rep(NA, nrow(goodLocs))
  for(i in 1:length(test)){
    test[i]<- bilinear(x= rev(y30m), y= x30m, z= z, 
                       x0= goodLocs[i,2], y0= goodLocs[i,1])$z
  }
  
  downscaled.z.vec<- test- elev.10m
  
  downscale30m<- downscaled.z.vec
  
  WSHtocompare<- c(as.matrix(extract(run10m,goodLocs)))
  
  length(which(downscale30m<0))
  
  downscale30m[which(downscale30m<0)]<- 0
  
  save(downscale30m, file= paste0("probabilisticDownscaling/data/",flow[f],"/simObs/downscale30mto10mAroundRS20.RData"))
  #downscaling improves performance compared to just adjusting here
  
  ptFinal<-proc.time()-pt
  time_downscale30mto10mAroundRS20<-ptFinal[3]
  save(time_downscale30mto10mAroundRS20, file= paste0("probabilisticDownscaling/data/",flow[f],"/simObs/time_downscale30mto10mAroundRS20.RData"))
  
  
  #compare downscaled projections to high resolution projections
  
  #correlation
  cor(downscale30m,WSHtocompare)
  #MAE
  mean(abs(downscale30m-WSHtocompare))
  #MSE
  mean((downscale30m-WSHtocompare)^2)
  
  #MAE
  posHeightInds<- which(WSHtocompare>0)
  mean(abs(downscale30m[posHeightInds]-WSHtocompare[posHeightInds])/WSHtocompare[posHeightInds])
  
  #compare adjusted projections to high resolution projections
  elev.30matFloodInds<- c(as.matrix(extract(dem30m,goodLocs)))
  val.30matFloodInds<- c(as.matrix(extract(run30m,goodLocs)))
  
  val.30mAdjatFloodInds<- val.30matFloodInds + elev.30matFloodInds - elev.10m
  
  #correlation
  cor(val.30mAdjatFloodInds,WSHtocompare)
  #MAE
  mean(abs(val.30mAdjatFloodInds-WSHtocompare))
  #MSE
  mean((val.30mAdjatFloodInds-WSHtocompare)^2)
  
  #MAPE
  posHeightInds<- which(WSHtocompare>0)
  mean(abs(val.30mAdjatFloodInds[posHeightInds]-WSHtocompare[posHeightInds])/WSHtocompare[posHeightInds])
  
  mean(WSHtocompare)
  var(WSHtocompare)
  
  quantile(WSHtocompare,c(.05,.06,.1,.25,.5,.75,.9,.95))
  
  
  discrepancy<- downscale30m-WSHtocompare
  
  VD30to10<- var(discrepancy)
  
  save(VD30to10, file= paste0("probabilisticDownscaling/data/",flow[f],"/simObs/VD30to10.RData"))
  
}