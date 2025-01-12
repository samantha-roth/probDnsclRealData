#get the total MAE and sensitivity for 10m resolution being downscaled to 5m resolution

rm(list=ls())

library(terra)

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

flood<- c("flood2014","flood2020","floodfuture")

for(f in 1:length(flood)){
  print(flood[f])
  
  ################################################################################
  #% in 95% PI bounds for wet cells
  
  load(paste0("data/",flood[f],"/bdsdownscale10mto5mAroundHWMs.RData"))
  
  #load flooded locations at the two lower resolutions being downscaled
  load(paste0("data/",flood[f],"/floodInds10mat5mAroundHWMs.RData"))
  
  #load region of interest coordinates
  load("data/boxAroundHWMs.5m.RData")
  
  #load calibrated 5m flood projections in region of interest
  load(paste0("data/",flood[f],"/vals5minBdsAroundHWMs.RData"))
  
  floodvals5mby10m<- vals5minBds[floodInds10mat5m]
  isBtwn5mby10m<- rep(NA,length(floodvals5mby10m))
  
  for(i in 1:length(floodvals5mby10m)){
    isBtwn5mby10m[i]<- floodvals5mby10m[i]>=bdsBox10m[i,1] & floodvals5mby10m[i]<=bdsBox10m[i,2]
  }
  
  mean(isBtwn5mby10m) #0.9758621
  
  ################################################################################
  
  #get MAE for wet cells
  
  load(paste0("data/",flood[f],"/downscale10mto5mAroundHWMs.RData"))
  
  #correlation
  cor(downscale10m,floodvals5mby10m)
  #MAE
  mean(abs(downscale10m-floodvals5mby10m))
  #MSE
  mean((downscale10m-floodvals5mby10m)^2)
  
  ################################################################################
  
  #get %in 95% PI bounds for dry cells
  
  run5m<- rast(paste0("data/Outputs5m/",flood[f],"/Run_1.asc"))
  coords.5m<- xyFromCell(run5m,1:ncell(run5m))
  
  #load cells of interest outside the low res flooded cells
  load(paste0("data/",flood[f],"/destInds10mat5m.RData"))
  coords.5mDest<- coords.5m[destInds10m,]
  
  #get the actual flood height at each destination location
  trueDestFloodHeights<- c(as.matrix(extract(run5m,coords.5mDest)))
  
  #load mean predictions at each cell
  load(paste0("data/",flood[f],"/shiftbyelevdnsclMeanAtDests_10mto5mElev_QGIS.RData"))
  
  
  #Now see if the mean lies with the center 95% of the distribution
  
  #load the estimated variance from comparing the downscaled projs to the HWMs
  load("data/varResHWM10mto5m.RData")
  
  #load the mean from the nearest downscaled flooded cell
  load(paste0("data/",flood[f],"/shiftbyelevdnsclFromSourceToDest10mto5m_QGIS.RData"))
  
  #what's the maximum number that should be plugged into the CDF?
  upper<- max(meanFromSourceToDest)+ 3*sqrt(varResHWM10m)
  
  valsToTest<- seq(0,round(upper+.01,2),by=.01)
  
  load(paste0("data/",flood[f],"/shiftbyelevdnsclCDFmat_10mto5mElev_QGIS.RData"))
  
  #95% confidence interval
  CI95mat<- matrix(NA,nrow= nrow(CDFmat), ncol=2)
  
  for(i in 1:nrow(CDFmat)){
    CI95mat[i,1]<- valsToTest[min(which(CDFmat[i,]>=.05))]
    CI95mat[i,2]<- valsToTest[min(which(CDFmat[i,]>=.95))]
  }
  
  inCI95Bds<- rep(NA,length(trueDestFloodHeights))
  for(i in 1:length(trueDestFloodHeights)){
    if(trueDestFloodHeights[i]>=CI95mat[i,1] & trueDestFloodHeights[i]<=CI95mat[i,2]){
      inCI95Bds[i]<- 1}
    else{
      inCI95Bds[i]<- 0}
  }
  #what % of the time is the truth within the 95% CI bounds? 
  mean(inCI95Bds) #0.9564926 #holy shit that's good
  
  
  ################################################################################
  
  #get MAE for dry cells
  
  #look at overall performance
  #MAE
  mean(abs(meanAtDests-trueDestFloodHeights)) #0.1531073
  
  #MSE
  mean((meanAtDests-trueDestFloodHeights)^2) #0.0525986
  
  ################################################################################
  
  #total % within 95% PI bounds
  
  PI95accuracy<- (sum(inCI95Bds)+ sum(isBtwn5mby10m))/(length(inCI95Bds)+ length(isBtwn5mby10m))
  print(paste0("95% Prediction Interval Coverage: ", PI95accuracy))
  
  MAE<- (sum(abs(meanAtDests-trueDestFloodHeights)) + sum(abs(downscale10m-floodvals5mby10m)))/(length(meanAtDests) + length(downscale10m))
  print(paste0("MAE: ", MAE))
  
  save(PI95accuracy,MAE,file=paste0("data/",flood[f],"/PI95accuracyMAE.RData"))
}

# "flood2014"
# "95% Prediction Interval Coverage: 0.980846697575435"
# "MAE: 0.0976865219590253"
# "flood2020"
# "95% Prediction Interval Coverage: 0.981074713080489"
# "MAE: 0.0939594490602081"
# "floodfuture"
# "95% Prediction Interval Coverage: 0.97689442882116"
# "MAE: 0.159626524733717"