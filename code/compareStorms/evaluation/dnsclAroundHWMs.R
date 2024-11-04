
rm(list=ls())

library(terra)

pt<- proc.time()

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

#load region of interest coordinates
load("data/boxAroundHWMs.5m.RData")

#load the DEM files at each resolution
dem5m<- rast("data/norristown_5m.asc")
dem10m<- rast("data/norristown_10m_new.asc")

flood<- c("flood2014","flood2020","floodfuture")

for(f in 1:length(flood)){
  
  #load flooded locations at the two lower resolutions being downscaled
  load(paste0("data/",flood[f],"/floodInds10mat5mAroundHWMs.RData"))
  
  #load calibrated flood projections at 5m in region of interest
  load(paste0("data/",flood[f],"/vals5minBdsAroundHWMs.RData"))
  
  #load the calibrated projections at each resolution
  run10m<- rast(paste0("data/Outputs10m/",flood[f],"/Run_1.asc"))
  
  ################################################################################
  #Next we use bilinear interpolation to get the WSH at 10m on 5m grid
  
  coords.10m<- xyFromCell(run10m,1:ncell(run10m))
  
  min.x10<- min(coords.10m[,1])
  min.y10<- min(coords.10m[,2])
  max.x10<- max(coords.10m[,1])
  max.y10<- max(coords.10m[,2])
  
  goodLocs<- coordsinBds.5m[floodInds10mat5m,]
  
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
  
  downscaled.z.vec<- test- elev.5m
  
  downscale10m<- downscaled.z.vec
  
  WSHtocompare<- vals5minBds[floodInds10mat5m]
  
  length(which(downscale10m<0))
  
  downscale10m[which(downscale10m<0)]<- 0
  
  save(downscale10m, file= paste0("data/",flood[f],"/downscale10mto5mAroundHWMs.RData"))
  #downscaling improves performance compared to just adjusting here
  
  ptFinal<-proc.time()-pt
  time_downscale10mAroundHWMs<-ptFinal[3]
  save(time_downscale10mAroundHWMs, file= paste0("data/",flood[f],"/time_downscale10mAroundHWMs.RData"))
  
  
  #compare downscaled projections to high resolution projections
  
  #correlation
  print(cor(downscale10m,WSHtocompare))
  #MAE
  print(mean(abs(downscale10m-WSHtocompare)))
  #MSE
  mean((downscale10m-WSHtocompare)^2)
  
  #MAE
  posHeightInds<- which(WSHtocompare>0)
  mean(abs(downscale10m[posHeightInds]-WSHtocompare[posHeightInds])/WSHtocompare[posHeightInds])
  
  #compare adjusted projections to high resolution projections
  elev.10matFloodInds<- c(as.matrix(extract(dem10m,goodLocs)))
  val.10matFloodInds<- c(as.matrix(extract(run10m,goodLocs)))
  
  val.10mAdjatFloodInds<- val.10matFloodInds + elev.10matFloodInds - elev.5m
  
  #correlation
  cor(val.10mAdjatFloodInds,WSHtocompare)
  #MAE
  mean(abs(val.10mAdjatFloodInds-WSHtocompare))
  #MSE
  mean((val.10mAdjatFloodInds-WSHtocompare)^2)
  
  #MAPE
  posHeightInds<- which(WSHtocompare>0)
  mean(abs(val.10mAdjatFloodInds[posHeightInds]-WSHtocompare[posHeightInds])/WSHtocompare[posHeightInds])
  
  mean(WSHtocompare)
  var(WSHtocompare)
  
  quantile(WSHtocompare,c(.05,.06,.1,.25,.5,.75,.9,.95))
  
  
  discrepancy<- downscale10m-WSHtocompare
  
  VD10to5<- var(discrepancy)
  
  save(VD10to5, file= paste0("data/",flood[f],"/VD10to5.RData"))
  
}
