#compare the expectation with the true value for flooded and nonflooded
#high resolution cells within a dry low resolution cell

rm(list=ls())

library(terra)

setwd("C:/Users/svr5482")

flow<- c("Q2559.8429","Q2503.2092","Q3681.19006")

for(f in 1:length(flow)){
  print(flow[f])
  
  run5m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs5m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  coords.5m<- xyFromCell(run5m,1:ncell(run5m))
  
  #load cells of interest outside the low res flooded cells
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/destInds10mat5m.RData"))
  coords.5mDest<- coords.5m[destInds10m,]
  
  #get the actual flood height at each destination location
  trueDestFloodHeights<- c(as.matrix(extract(run5m,coords.5mDest)))
  
  #load indices of true positive, false negative, true negative destination cells
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/estProbFloodIndsofInterest_10mto5mElev.RData"))
  
  #load mean predictions at each cell
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/shiftbyelevdnsclMeanAtDests_10mto5mElev.RData"))
  TPmean<- meanAtDests[indsFloodCorrect]
  
  TNmean<- meanAtDests[indsNotFloodCorrect]
  FNmean<- meanAtDests[indsFalseNeg]
  
  TPtruth<- trueDestFloodHeights[indsFloodCorrect]
  TNtruth<- trueDestFloodHeights[indsNotFloodCorrect]
  FNtruth<- trueDestFloodHeights[indsFalseNeg]
  
  #first get MAE, MSE
  
  #MAE
  mean(abs(TPmean-TPtruth)) #0.5165739
  mean(abs(TNmean-TNtruth)) #0.1057225
  mean(abs(FNmean-FNtruth)) #0.2456829
  
  #MSE
  mean((TPmean-TPtruth)^2) #0.3811854
  mean((TNmean-TNtruth)^2) #0.01156287
  mean((FNmean-FNtruth)^2) #0.1195728
  
  #look at overall performance
  #MAE
  print(paste0("MAE: ", mean(abs(meanAtDests-trueDestFloodHeights)))) #0.1531073
  
  #MSE
  mean((meanAtDests-trueDestFloodHeights)^2) #0.0525986
  
  #get upper bound for values to approximate CDF at
  #load the mean given the flood height does not come from the point mass at zero
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/shiftbyelevdnsclFromSourceToDest10mto5m.RData"))
  #load the estimated variance from comparing the downscaled projs to the HWMs
  load("probabilisticDownscaling/data/simObs/varResHWM10mto5m.RData")
  
  upper<- max(meanFromSourceToDest)+ 3*sqrt(varResHWM10m)
  
  valsToTest<- seq(0,round(upper+.01,2),by=.01)
  
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/shiftbyelevdnsclCDFmat_10mto5mElev.RData"))
  
  
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
  print(paste0("% in bounds: ", mean(inCI95Bds))) #0.9564926 #holy shit that's good
  
  #in the dry region our approach has 1/2 the MSE and 1/3 the MAE of the simple SLR approach
  #also 3% higher confidence interval coverage
  
  length(trueDestFloodHeights)
}

# "Q2559.8429"
# "MAE: 0.00618634597132498"
# "% in bounds: 1"
# "Q2503.2092"
# "MAE: 0.0065863386315848"
# "% in bounds: 1"
# "Q3681.19006"
# "MAE: 0.00790697244771804"
# "% in bounds: 1"