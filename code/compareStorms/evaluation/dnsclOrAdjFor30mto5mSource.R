rm(list=ls())

library(terra)

setwd("C:/Users/svr5482")

pt<- proc.time()

#load the DEM files at each resolution
dem5m<- rast("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/LISFLOOD/norristown_5m.asc")
dem30m<- rast("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/LISFLOOD/norristown_30m_new.asc")

flow<- c("Q2559.8429","Q2503.2092","Q3681.19006")

for(f in 1:length(flow)){
  
  print(flow[f])
  
  #load coordinates of sources for the destination 5m cells
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/sourceIndsCoordsForDests30mto5m.RData"))
  
  #load the calibrated projections at each resolution
  run30m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs30m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  run5m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs5m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  
  ################################################################################
  #Next we use bilinear interpolation to get the WSH at 30m on 5m grid
  
  coords.30m<- xyFromCell(run30m,1:ncell(run30m))
  
  min.x30<- min(coords.30m[,1])
  min.y30<- min(coords.30m[,2])
  max.x30<- max(coords.30m[,1])
  max.y30<- max(coords.30m[,2])
  
  goodLocs<- sourceCoordsUnique
  
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
  
  WSE.dnscl<- test
  save(WSE.dnscl,file=paste0("probabilisticDownscaling/data/",flow[f],"/WSE.dnsclAtSource30mto5m.RData"))
  
  downscaled.z.vec<- test- elev.5m
  
  downscale30m<- downscaled.z.vec
  
  WSHtocompare<- c(as.matrix(extract(run5m,goodLocs)))
  
  length(which(downscale30m<0))
  
  downscale30m[which(downscale30m<0)]<- 0
  
  
  save(downscale30m, file= paste0("probabilisticDownscaling/data/",flow[f],"/simObs/downscale30mto5mAtSource.RData"))
  #downscaling improves performance compared to just adjusting
  #in terms of correlation and MSE but not MAE
  
  ptFinal<-proc.time()-pt
  time_downscale30mAtSource<-ptFinal[3]
  save(time_downscale30mAtSource, file= paste0("probabilisticDownscaling/data/",flow[f],"/simObs/time_downscale30mto5mAtSource.RData"))
  
  
  #compare downscaled projections to high resolution projections
  
  print("downscaled")
  
  #correlation
  print(cor(downscale30m,WSHtocompare))
  #MAE
  print(mean(abs(downscale30m-WSHtocompare)))
  #MSE
  print(mean((downscale30m-WSHtocompare)^2))
  
  #compare adjusted projections to high resolution projections
  elev.30mAtSource<- c(as.matrix(extract(dem30m,goodLocs)))
  val.30mAtSource<- c(as.matrix(extract(run30m,goodLocs)))
  
  val.30mAdjAtSource<- val.30mAtSource + elev.30mAtSource - elev.5m
  save(val.30mAdjAtSource,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/val.30mto5mAdjAtSource.RData"))
  
  print("adjusted")
  
  #correlation
  print(cor(val.30mAdjAtSource,WSHtocompare))
  #MAE
  print(mean(abs(val.30mAdjAtSource-WSHtocompare)))
  #MSE
  print(mean((val.30mAdjAtSource-WSHtocompare)^2))
  
}

#[1] "Q2559.8429"
#[1] "downscaled"
#[1] 0.8027474
#[1] 0.185518
#[1] 0.06416595
#[1] "adjusted"
#[1] 0.6492181
#[1] 0.2599942
#[1] 0.2247085
#[1] "Q2503.2092"
#[1] "downscaled"
#[1] 0.7004594
#[1] 0.229333
#[1] 0.08323812
#[1] "adjusted"
#[1] 0.5895701
#[1] 0.2305689
#[1] 0.1859101
#[1] "Q3681.19006"
#[1] "downscaled"
#[1] 0.8332477
#[1] 0.4873956
#[1] 0.4788106
#[1] "adjusted"
#[1] 0.7646505
#[1] 0.4832983
#[1] 1.180609