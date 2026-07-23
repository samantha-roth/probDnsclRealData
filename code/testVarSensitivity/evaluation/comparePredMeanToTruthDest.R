#compare the expectation with the true value for flooded and nonflooded
#high resolution cells within a dry low resolution cell

rm(list=ls())

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

library(terra)
library(crch)

n_obs=5


run5m<- rast("data/Outputs5m/Run_1.asc")
load("data/coords.5m.RData")

#load cells of interest outside the low res flooded cells
load("data/destInds10mat5m.RData")
coords.5mDest<- coords.5m[destInds10m,]

#get the actual flood height at each destination location
trueDestFloodHeights<- c(as.matrix(extract(run5m,coords.5mDest)))

#load the mean from the nearest downscaled flooded cell
load("data/shiftbyelevdnsclFromSourceToDest10mto5m_QGIS.RData")

#load the sampled variances from the empirical distribution 
load("data/var_samples")

for(v in 1: length(var_samples)){
  
  varResHWM10m<- var_samples[v]
  
  print(paste0("var sample= ",var_samples[v]))
  
  #load indices of true positive, false negative, true negative destination cells
  load(paste0("data/var_sample",v,"/estProbFloodIndsofInterest_10mto5mElev_QGIS.RData"))
  
  #load mean predictions at each cell
  load(paste0("data/var_sample",v,"/shiftbyelevdnsclMeanAtDests_10mto5mElev_QGIS.RData"))
  
  TPmean<- meanAtDests[indsFloodCorrect]
  TNmean<- meanAtDests[indsNotFloodCorrect]
  FNmean<- meanAtDests[indsFalseNeg]
  
  TPtruth<- trueDestFloodHeights[indsFloodCorrect]
  TNtruth<- trueDestFloodHeights[indsNotFloodCorrect]
  FNtruth<- trueDestFloodHeights[indsFalseNeg]
  
  
  #first get MAE, MSE
  
  #MAE
  mean(abs(TPmean-TPtruth)) #0.1013988
  mean(abs(TNmean-TNtruth)) #0.007204583
  mean(abs(FNmean-FNtruth)) #0.05228772
  
  #look at overall performance
  #MAE
  print(paste0("MAE: ",mean(abs(meanAtDests-trueDestFloodHeights)))) #0.009710433
  
  #Now see if the mean lies with the center 95% of the distribution
  
  upper<- max(meanFromSourceToDest)+ 3*sqrt(varResHWM10m)
  
  valsToTest<- seq(0,round(upper+.01,2),by=.01)
  
  load(paste0("data/var_sample",v,"/shiftbyelevdnsclCDFmat_10mto5mElev_QGIS.RData"))
  
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
  print(paste0("95% PI coverage: ", mean(inCI95Bds))) #0.9992657
  
}

