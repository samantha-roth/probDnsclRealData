
rm(list=ls())

library(terra)

pt<- proc.time()

setwd("C:/Users/svr5482")

#load region of interest coordinates
load("probabilisticDownscaling/data/simObs/boxAroundRS20.5m.RData")

#load the DEM files at each resolution
dem5m<- rast("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/LISFLOOD/norristown_5m.asc")
dem50m<- rast("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/LISFLOOD/norristown_50m_new.asc")


flow<- c("Q2559.8429","Q2503.2092","Q3681.19006")

for(f in 1:length(flow)){
  
  #load flooded locations at the two lower resolutions being downscaled
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/floodInds50mat5mAroundRS20.RData"))
  
  #load calibrated flood projections at 5m in region of interest
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/vals5minBdsAroundRS20.RData"))
  
  #load the calibrated projections at each resolution
  run50m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs50m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  
  ################################################################################
  #Next we use bilinear interpolation to get the WSH at 50m on 5m grid
  
  coords.50m<- xyFromCell(run50m,1:ncell(run50m))
  
  min.x50<- min(coords.50m[,1])
  min.y50<- min(coords.50m[,2])
  max.x50<- max(coords.50m[,1])
  max.y50<- max(coords.50m[,2])
  
  goodLocs<- coordsinBds.5m[floodInds50mat5m,]
  
  y5m<- unique(goodLocs[,2])
  x5m<- unique(goodLocs[,1])
  y50m<- unique(coords.50m[,2])
  x50m<- unique(coords.50m[,1])
  
  nx5m<- length(x5m)
  ny5m<- length(y5m)
  nx50m<- length(x50m)
  ny50m<- length(y50m)
  
  library(akima)
  
  elev.5m<- c(as.matrix(extract(dem5m,goodLocs)))
  elev.50m<- c(as.matrix(extract(dem50m,coords.50m)))
  wsh.50m<- c(as.matrix(extract(run50m,coords.50m)))
  vals.50m<- wsh.50m + elev.50m
  
  z1= matrix(vals.50m, nrow= ny50m, ncol= nx50m, byrow= TRUE)
  
  z= matrix(NA,nrow= ny50m, ncol= nx50m)
  for(j in 1:nx50m){ z[,j]<- rev(z1[,j]) }
  
  test<- rep(NA, nrow(goodLocs))
  for(i in 1:length(test)){
    test[i]<- bilinear(x= rev(y50m), y= x50m, z= z, 
                       x0= goodLocs[i,2], y0= goodLocs[i,1])$z
  }
  
  downscaled.z.vec<- test- elev.5m
  
  downscale50m<- downscaled.z.vec
  
  WSHtocompare<- vals5minBds[floodInds50mat5m]
  
  length(which(downscale50m<0))
  
  downscale50m[which(downscale50m<0)]<- 0
  
  save(downscale50m, file= paste0("probabilisticDownscaling/data/",flow[f],"/simObs/downscale50mto5mAroundRS20.RData"))
  #downscaling improves performance compared to just adjusting here
  
  ptFinal<-proc.time()-pt
  time_downscale50mAroundRS20<-ptFinal[3]
  save(time_downscale50mAroundRS20, file= paste0("probabilisticDownscaling/data/",flow[f],"/simObs/time_downscale50mAroundRS20.RData"))
  
  
  #compare downscaled projections to high resolution projections
  
  #correlation
  cor(downscale50m,WSHtocompare)
  #MAE
  mean(abs(downscale50m-WSHtocompare))
  #MSE
  mean((downscale50m-WSHtocompare)^2)
  
  #MAE
  posHeightInds<- which(WSHtocompare>0)
  mean(abs(downscale50m[posHeightInds]-WSHtocompare[posHeightInds])/WSHtocompare[posHeightInds])
  
  #compare adjusted projections to high resolution projections
  elev.50matFloodInds<- c(as.matrix(extract(dem50m,goodLocs)))
  val.50matFloodInds<- c(as.matrix(extract(run50m,goodLocs)))
  
  val.50mAdjatFloodInds<- val.50matFloodInds + elev.50matFloodInds - elev.5m
  
  #correlation
  cor(val.50mAdjatFloodInds,WSHtocompare)
  #MAE
  mean(abs(val.50mAdjatFloodInds-WSHtocompare))
  #MSE
  mean((val.50mAdjatFloodInds-WSHtocompare)^2)
  
  #MAPE
  posHeightInds<- which(WSHtocompare>0)
  mean(abs(val.50mAdjatFloodInds[posHeightInds]-WSHtocompare[posHeightInds])/WSHtocompare[posHeightInds])
  
  mean(WSHtocompare)
  var(WSHtocompare)
  
  quantile(WSHtocompare,c(.05,.06,.1,.25,.5,.75,.9,.95))
  
  
  discrepancy<- downscale50m-WSHtocompare
  
  VD50to5<- var(discrepancy)
  
  save(VD50to5, file= paste0("probabilisticDownscaling/data/",flow[f],"/simObs/VD50to5.RData"))
  
}
