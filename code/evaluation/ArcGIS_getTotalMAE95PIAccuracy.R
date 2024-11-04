#get the total MAE and sensitivity for 10m resolution being downscaled to 5m resolution

rm(list=ls())

setwd("C:/Users/svr5482")

################################################################################
#% in 95% PI bounds for wet cells

load("probDnsclRealData/data/bdsdownscale10mto5mAroundHWMs.RData")

#load flooded locations at the two lower resolutions being downscaled
load("probDnsclRealData/data/floodInds10mat5mAroundHWMs.RData")

#load region of interest coordinates
load("probDnsclRealData/data/boxAroundHWMs.5m.RData")

#load calibrated 5m flood projections in region of interest
load("probDnsclRealData/data/vals5minBdsAroundHWMs.RData")

floodvals5mby10m<- vals5minBds[floodInds10mat5m]
isBtwn5mby10m<- rep(NA,length(floodvals5mby10m))

for(i in 1:length(floodvals5mby10m)){
  isBtwn5mby10m[i]<- floodvals5mby10m[i]>=bdsBox10m[i,1] & floodvals5mby10m[i]<=bdsBox10m[i,2]
}

mean(isBtwn5mby10m) #0.9658885

################################################################################

#get MAE for wet cells

load("probDnsclRealData/data/downscale10mto5mAroundHWMs.RData")

#correlation
cor(downscale10m,floodvals5mby10m) #0.988139
#MAE
mean(abs(downscale10m-floodvals5mby10m)) #0.2193905
#MSE
mean((downscale10m-floodvals5mby10m)^2)#0.2456247

################################################################################

#get %in 95% PI bounds for dry cells

run5m<- rast("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs5m/Norristown/nCh/PostMedCh/Extent/Run_1.asc")
coords.5m<- xyFromCell(run5m,1:ncell(run5m))

#load cells of interest outside the low res flooded cells
load("probDnsclRealData/data/destInds10mat5m.RData")
coords.5mDest<- coords.5m[destInds10m,]

#get the actual flood height at each destination location
trueDestFloodHeights<- c(as.matrix(extract(run5m,coords.5mDest)))

#load mean predictions at each cell
load("probDnsclRealData/data/shiftbyelevdnsclMeanAtDests_10mto5mElev.RData")


#Now see if the mean lies with the center 95% of the distribution

#load the estimated variance from comparing the downscaled projs to the HWMs
load("probDnsclRealData/data/varResHWM10mto5m.RData")

#load the mean from the nearest downscaled flooded cell
load("probDnsclRealData/data/shiftbyelevdnsclFromSourceToDest10mto5m.RData")

#what's the maximum number that should be plugged into the CDF?
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


################################################################################

#get MAE for dry cells

#look at overall performance
#MAE
mean(abs(meanAtDests-trueDestFloodHeights)) #0.00971371

#MSE
mean((meanAtDests-trueDestFloodHeights)^2) #0.0009926451

################################################################################

#total % within 95% PI bounds

PI95accuracy<- (sum(inCI95Bds)+ sum(isBtwn5mby10m))/(length(inCI95Bds)+ length(isBtwn5mby10m))
PI95accuracy # 0.9797066

MAE<- (sum(abs(meanAtDests-trueDestFloodHeights)) + sum(abs(downscale10m-floodvals5mby10m)))/(length(meanAtDests) + length(downscale10m))
MAE #0.1325842
