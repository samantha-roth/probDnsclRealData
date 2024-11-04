#compare the expectation with the true value for flooded and nonflooded
#high resolution cells within a dry low resolution cell

rm(list=ls())

library(terra)

setwd("C:/Users/svr5482")

run5m<- rast("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs5m/Norristown/nCh/PostMedCh/Extent/Run_1.asc")
load("probDnsclRealData/data/coords.5m.RData")

#load cells of interest outside the low res flooded cells
load("probDnsclRealData/data/destInds10mat5m.RData")
coords.5mDest<- coords.5m[destInds10m,]

#get the actual flood height at each destination location
trueDestFloodHeights<- c(as.matrix(extract(run5m,coords.5mDest)))

#load indices of true positive, false negative, true negative destination cells
load("probDnsclRealData/data/estProbFloodIndsofInterest_10mto5mElev.RData")

#load mean predictions at each cell
load("probDnsclRealData/data/shiftbyelevdnsclMeanAtDests_10mto5mElev.RData")

TPmean<- meanAtDests[indsFloodCorrect]
TNmean<- meanAtDests[indsNotFloodCorrect]
FNmean<- meanAtDests[indsFalseNeg]

TPtruth<- trueDestFloodHeights[indsFloodCorrect]
TNtruth<- trueDestFloodHeights[indsNotFloodCorrect]
FNtruth<- trueDestFloodHeights[indsFalseNeg]


#first get MAE, MSE

#MAE
mean(abs(TPmean-TPtruth)) #0.1016225
mean(abs(TNmean-TNtruth)) #0.007204583
mean(abs(FNmean-FNtruth)) #0.05229381

#look at overall performance
#MAE
mean(abs(meanAtDests-trueDestFloodHeights)) #0.00971371

#Now see if the mean lies with the center 95% of the distribution

#load the mean from the nearest downscaled flooded cell
load("probDnsclRealData/data/shiftbyelevdnsclFromSourceToDest10mto5m.RData")

#load the estimated variance from comparing the downscaled projs to the HWMs
load("probDnsclRealData/data/varResHWM10mto5m.RData")

upper<- max(meanFromSourceToDest)+ 3*sqrt(varResHWM10m)

valsToTest<- seq(0,round(upper+.01,2),by=.01)

load("probDnsclRealData/data/shiftbyelevdnsclCDFmat_10mto5mElev.RData")


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
mean(inCI95Bds) #0.9992657

#in the dry region our approach has 1/2 the MSE and 1/3 the MAE of the simple SLR approach
#also 3% higher confidence interval coverage

length(trueDestFloodHeights)