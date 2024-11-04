#get the total MAE and sensitivity for 10m resolution being downscaled to 5m resolution

rm(list=ls())

setwd("C:/Users/svr5482")

flow<- c("Q2559.8429","Q2503.2092","Q3681.19006")

for(f in 1:length(flow)){
  print(flow[f])
  
  ################################################################################
  #% in 95% PI bounds for wet cells
  
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/interpVarDnsclErrbyMeanAbsElevDiff/bdsdownscale10mto5mAroundRS20.RData"))
  
  #load flooded locations at the two lower resolutions being downscaled
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/floodInds10mat5mAroundRS20.RData"))
  
  #load region of interest coordinates
  load("probabilisticDownscaling/data/simObs/boxAroundRS20.5m.RData")
  
  #load calibrated 5m flood projections in region of interest
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/vals5minBdsAroundRS20.RData"))
  
  floodvals5mby10m<- vals5minBds[floodInds10mat5m]
  isBtwn5mby10m<- rep(NA,length(floodvals5mby10m))
  
  for(i in 1:length(floodvals5mby10m)){
    isBtwn5mby10m[i]<- floodvals5mby10m[i]>=bdsBox10m[i,1] & floodvals5mby10m[i]<=bdsBox10m[i,2]
  }
  
  mean(isBtwn5mby10m) #0.9758621
  
  ################################################################################
  
  #get MAE for wet cells
  
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/downscale10mto5mAroundRS20.RData"))
  
  #correlation
  cor(downscale10m,floodvals5mby10m)
  #MAE
  mean(abs(downscale10m-floodvals5mby10m))
  #MSE
  mean((downscale10m-floodvals5mby10m)^2)
  
  ################################################################################
  
  #get %in 95% PI bounds for dry cells
  
  run5m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs5m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  coords.5m<- xyFromCell(run5m,1:ncell(run5m))
  
  #load cells of interest outside the low res flooded cells
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/destInds10mat5m.RData"))
  coords.5mDest<- coords.5m[destInds10m,]
  
  #get the actual flood height at each destination location
  trueDestFloodHeights<- c(as.matrix(extract(run5m,coords.5mDest)))
  
  #load mean predictions at each cell
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/interpVarDnsclErrbyMeanAbsElevDiff/shiftbyelevdnsclMeanAtDests_10mto5m.RData"))
  
  
  #Now see if the mean lies with the center 95% of the distribution
  
  #load the estimated variance from comparing the downscaled projs to the HWMs
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/interpVarDnsclErrbyMeanAbsElevDiff/predVD10mto5m.RData"))
  
  #load the mean from the nearest downscaled flooded cell
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/shiftbyelevdnsclFromSourceToDest10mto5m.RData"))
  
  #what's the maximum number that should be plugged into the CDF?
  upper<- max(meanFromSourceToDest)+ 3*sqrt(predVD10mto5m)
  
  valsToTest<- seq(0,round(upper+.01,2),by=.01)
  
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/interpVarDnsclErrbyMeanAbsElevDiff/shiftbyelevdnsclCDFmat_10mto5m.RData"))
  
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
  
}

#[1] "Q2559.8429"
#[1] "95% Prediction Interval Coverage: 0.99780701754386"
#[1] "MAE: 0.03188052080972"
#[1] "Q2503.2092"
#[1] "95% Prediction Interval Coverage: 0.997563352826511"
#[1] "MAE: 0.0250991112464723"
#[1] "Q3681.19006"
#[1] "95% Prediction Interval Coverage: 0.998294346978557"
#[1] "MAE: 0.0542118307769209"