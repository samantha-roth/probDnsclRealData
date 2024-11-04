rm(list=ls())

library(terra)

setwd("C:/Users/svr5482")

pt<- proc.time()

#load the DEM files at each resolution
dem5m<- rast("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/LISFLOOD/norristown_5m.asc")
dem10m<- rast("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/LISFLOOD/norristown_10m_new.asc")

flood<- c("flood2014","flood2020","floodfuture")

for(f in 1:length(flood)){
  
  print(flood[f])
  
  #load coordinates of sources for the destination 5m cells
  load(paste0("probDnsclRealData/data/",flood[f],"/sourceIndsCoordsForDests10mto5m.RData"))
  
  #load the calibrated projections at each resolution
  run10m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs10m/Norristown/nCh/PostMedCh/",flood[f],"/Extent/Run_1.asc"))
  run5m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs5m/Norristown/nCh/PostMedCh/",flood[f],"/Extent/Run_1.asc"))
  
  ################################################################################
  #Next we use bilinear interpolation to get the WSH at 10m on 5m grid
  
  coords.10m<- xyFromCell(run10m,1:ncell(run10m))
  
  min.x10<- min(coords.10m[,1])
  min.y10<- min(coords.10m[,2])
  max.x10<- max(coords.10m[,1])
  max.y10<- max(coords.10m[,2])
  
  goodLocs<- sourceCoordsUnique
  
  y5m<- unique(goodLocs[,2])
  x5m<- unique(goodLocs[,1])
  y10m<- unique(coords.10m[,2])
  x10m<- unique(coords.10m[,1])
  
  nx5m<- length(x5m)
  ny5m<- length(y5m)
  nx10m<- length(x10m)
  ny10m<- length(y10m)
  
  library(akima)
  
  elev.5m<- c(as.matrix(extract(dem5m,goodLocs)))
  elev.10m<- c(as.matrix(extract(dem10m,coords.10m)))
  wsh.10m<- c(as.matrix(extract(run10m,coords.10m)))
  vals.10m<- wsh.10m + elev.10m
  
  z1= matrix(vals.10m, nrow= ny10m, ncol= nx10m, byrow= TRUE)
  
  z= matrix(NA,nrow= ny10m, ncol= nx10m)
  for(j in 1:nx10m){ z[,j]<- rev(z1[,j]) }
  
  test<- rep(NA, nrow(goodLocs))
  for(i in 1:length(test)){
    test[i]<- bilinear(x= rev(y10m), y= x10m, z= z, 
                       x0= goodLocs[i,2], y0= goodLocs[i,1])$z
  }
  
  WSE.dnscl<- test
  save(WSE.dnscl,file=paste0("probDnsclRealData/data/",flood[f],"/WSE.dnsclAtSource10mto5m.RData"))
  
  downscaled.z.vec<- test- elev.5m
  
  downscale10m<- downscaled.z.vec
  
  WSHtocompare<- c(as.matrix(extract(run5m,goodLocs)))
  
  length(which(downscale10m<0))
  
  downscale10m[which(downscale10m<0)]<- 0
  
  
  save(downscale10m, file= paste0("probDnsclRealData/data/",flood[f],"/downscale10mto5mAtSource.RData"))
  #downscaling improves performance compared to just adjusting
  #in terms of correlation and MSE but not MAE
  
  ptFinal<-proc.time()-pt
  time_downscale10mAtSource<-ptFinal[3]
  save(time_downscale10mAtSource, file= paste0("probDnsclRealData/data/",flood[f],"/time_downscale10mto5mAtSource.RData"))
  
  
  #compare downscaled projections to high resolution projections
  
  print("downscaled")
  
  #correlation
  print(cor(downscale10m,WSHtocompare))
  #MAE
  print(mean(abs(downscale10m-WSHtocompare)))
  #MSE
  print(mean((downscale10m-WSHtocompare)^2))
  
  #compare adjusted projections to high resolution projections
  elev.10mAtSource<- c(as.matrix(extract(dem10m,goodLocs)))
  val.10mAtSource<- c(as.matrix(extract(run10m,goodLocs)))
  
  val.10mAdjAtSource<- val.10mAtSource + elev.10mAtSource - elev.5m
  save(val.10mAdjAtSource,file=paste0("probDnsclRealData/data/",flood[f],"/val.10mto5mAdjAtSource.RData"))
  
  print("adjusted")
  
  #correlation
  print(cor(val.10mAdjAtSource,WSHtocompare))
  #MAE
  print(mean(abs(val.10mAdjAtSource-WSHtocompare)))
  #MSE
  print(mean((val.10mAdjAtSource-WSHtocompare)^2))
  
}

#[1] "Q2559.8429"
#[1] "downscaled"
#[1] 0.942414
#[1] 0.04353617
#[1] 0.006047558
#[1] "adjusted"
#[1] 0.5343007
#[1] 0.2114141
#[1] 0.3141153
#[1] "Q2503.2092"
#[1] "downscaled"
#[1] 0.919327
#[1] 0.04589445
#[1] 0.006888059
#[1] "adjusted"
#[1] 0.5101235
#[1] 0.2155786
#[1] 0.3579353
#[1] "Q3681.19006"
#[1] "downscaled"
#[1] 0.9629315
#[1] 0.05215661
#[1] 0.01361118
#[1] "adjusted"
#[1] 0.7885376
#[1] 0.3203728
#[1] 0.3025068