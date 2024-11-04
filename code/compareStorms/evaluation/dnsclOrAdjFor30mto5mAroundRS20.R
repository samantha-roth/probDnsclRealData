
rm(list=ls())

library(terra)

pt<- proc.time()

setwd("C:/Users/svr5482")

#load region of interest coordinates
load("probabilisticDownscaling/data/simObs/boxAroundRS20.5m.RData")

#load the DEM files at each resolution
dem5m<- rast("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/LISFLOOD/norristown_5m.asc")
dem30m<- rast("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/LISFLOOD/norristown_30m_new.asc")


flow<- c("Q2559.8429","Q2503.2092","Q3681.19006")

for(f in 1:length(flow)){
  
  #load flooded locations at the two lower resolutions being downscaled
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/floodInds30mat5mAroundRS20.RData"))
  
  #load calibrated flood projections at 5m in region of interest
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/vals5minBdsAroundRS20.RData"))
  
  #load the calibrated projections at each resolution
  run30m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs30m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  
  ################################################################################
  #Next we use bilinear interpolation to get the WSH at 30m on 5m grid
  
  coords.30m<- xyFromCell(run30m,1:ncell(run30m))
  
  min.x30<- min(coords.30m[,1])
  min.y30<- min(coords.30m[,2])
  max.x30<- max(coords.30m[,1])
  max.y30<- max(coords.30m[,2])
  
  goodLocs<- coordsinBds.5m[floodInds30mat5m,]
  
  y5m<- unique(goodLocs[,2])
  x5m<- unique(goodLocs[,1])
  y30m<- unique(coords.30m[,2])
  x30m<- unique(coords.30m[,1])
  
  nx5m<- length(x5m)
  ny5m<- length(y5m)
  nx30m<- length(x30m)
  ny30m<- length(y30m)
  
  library(akima)
  
  elev.5m<- c(as.matrix(extract(dem5m,goodLocs)))
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
  
  downscaled.z.vec<- test- elev.5m
  
  downscale30m<- downscaled.z.vec
  
  WSHtocompare<- vals5minBds[floodInds30mat5m]
  
  length(which(downscale30m<0))
  
  downscale30m[which(downscale30m<0)]<- 0
  
  save(downscale30m, file= paste0("probabilisticDownscaling/data/",flow[f],"/simObs/downscale30mto5mAroundRS20.RData"))
  #downscaling improves performance compared to just adjusting here
  
  ptFinal<-proc.time()-pt
  time_downscale30mAroundRS20<-ptFinal[3]
  save(time_downscale30mAroundRS20, file= paste0("probabilisticDownscaling/data/",flow[f],"/simObs/time_downscale30mAroundRS20.RData"))
  
  
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
  
  val.30mAdjatFloodInds<- val.30matFloodInds + elev.30matFloodInds - elev.5m
  
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
  
  VD30to5<- var(discrepancy)
  
  save(VD30to5, file= paste0("probabilisticDownscaling/data/",flow[f],"/simObs/VD30to5.RData"))
  
}
