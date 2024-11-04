rm(list=ls())

library(terra)

setwd("C:/Users/svr5482")

pt<- proc.time()

#load the DEM files at each resolution
dem5m<- rast("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/LISFLOOD/norristown_5m.asc")
dem50m<- rast("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/LISFLOOD/norristown_50m_new.asc")

flow<- c("Q2559.8429","Q2503.2092","Q3681.19006")

for(f in 1:length(flow)){
  
  print(flow[f])
  
  #load coordinates of sources for the destination 5m cells
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/sourceIndsCoordsForDests50mto5m.RData"))
  
  #load the calibrated projections at each resolution
  run50m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs50m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  run5m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs5m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  
  ################################################################################
  #Next we use bilinear interpolation to get the WSH at 50m on 5m grid
  
  coords.50m<- xyFromCell(run50m,1:ncell(run50m))
  
  min.x50<- min(coords.50m[,1])
  min.y50<- min(coords.50m[,2])
  max.x50<- max(coords.50m[,1])
  max.y50<- max(coords.50m[,2])
  
  goodLocs<- sourceCoordsUnique
  
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
  
  WSE.dnscl<- test
  save(WSE.dnscl,file=paste0("probabilisticDownscaling/data/",flow[f],"/WSE.dnsclAtSource50mto5m.RData"))
  
  downscaled.z.vec<- test- elev.5m
  
  downscale50m<- downscaled.z.vec
  
  WSHtocompare<- c(as.matrix(extract(run5m,goodLocs)))
  
  length(which(downscale50m<0))
  
  downscale50m[which(downscale50m<0)]<- 0
  
  
  save(downscale50m, file= paste0("probabilisticDownscaling/data/",flow[f],"/simObs/downscale50mto5mAtSource.RData"))
  #downscaling improves performance compared to just adjusting
  #in terms of correlation and MSE but not MAE
  
  ptFinal<-proc.time()-pt
  time_downscale50mAtSource<-ptFinal[3]
  save(time_downscale50mAtSource, file= paste0("probabilisticDownscaling/data/",flow[f],"/simObs/time_downscale50mto5mAtSource.RData"))
  
  
  #compare downscaled projections to high resolution projections
  
  print("downscaled")
  
  #correlation
  print(cor(downscale50m,WSHtocompare))
  #MAE
  print(mean(abs(downscale50m-WSHtocompare)))
  #MSE
  print(mean((downscale50m-WSHtocompare)^2))
  
  #compare adjusted projections to high resolution projections
  elev.50mAtSource<- c(as.matrix(extract(dem50m,goodLocs)))
  val.50mAtSource<- c(as.matrix(extract(run50m,goodLocs)))
  
  val.50mAdjAtSource<- val.50mAtSource + elev.50mAtSource - elev.5m
  save(val.50mAdjAtSource,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/val.50mto5mAdjAtSource.RData"))
  
  print("adjusted")
  
  #correlation
  print(cor(val.50mAdjAtSource,WSHtocompare))
  #MAE
  print(mean(abs(val.50mAdjAtSource-WSHtocompare)))
  #MSE
  print(mean((val.50mAdjAtSource-WSHtocompare)^2))
  
}

#[1] "Q2559.8429"
#[1] "downscaled"
#[1] 0.4431588
#[1] 0.3884446
#[1] 0.3540573
#[1] "adjusted"
#[1] 0.6424002
#[1] 0.256172
#[1] 0.2300333
#[1] "Q2503.2092"
#[1] "downscaled"
#[1] 0.529471
#[1] 0.3413669
#[1] 0.2651408
#[1] "adjusted"
#[1] 0.5960735
#[1] 0.293498
#[1] 0.258372
#[1] "Q3681.19006"
#[1] "downscaled"
#[1] 0.9681615
#[1] 0.3970542
#[1] 0.2301198
#[1] "adjusted"
#[1] 0.8750342
#[1] 0.4315037
#[1] 0.6119479