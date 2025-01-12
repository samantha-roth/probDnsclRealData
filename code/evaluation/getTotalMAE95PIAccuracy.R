#get the total MAE and sensitivity for 10m resolution being downscaled to 5m resolution

rm(list=ls())
library(terra)
dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

################################################################################
#% in 95% PI bounds for wet cells

load("data/bdsdownscale10mto5mAroundHWMs.RData")

#load flooded locations at the lower resolution being downscaled
load("data/floodInds10mat5mAroundHWMs.RData")
 
#load region of interest coordinates
load("data/boxAroundHWMs.5m.RData")

#load calibrated 5m flood projections in region of interest
load("data/vals5minBdsAroundHWMs.RData")

floodvals5mby10m<- vals5minBds[floodInds10mat5m]
isBtwn5mby10m<- rep(NA,length(floodvals5mby10m))

for(i in 1:length(floodvals5mby10m)){
  isBtwn5mby10m[i]<- floodvals5mby10m[i]>=bdsBox10m[i,1] & floodvals5mby10m[i]<=bdsBox10m[i,2]
}

mean(isBtwn5mby10m) #0.9658885

################################################################################

#get MAE for wet cells

load("data/downscale10mto5mAroundHWMs.RData")

#correlation
cor(downscale10m,floodvals5mby10m) #0.988139
#MAE
mean(abs(downscale10m-floodvals5mby10m)) #0.2193905
#MSE
mean((downscale10m-floodvals5mby10m)^2)#0.2456247

################################################################################

#get %in 95% PI bounds for dry cells

run5m<- rast("data/Outputs5m/Run_1.asc")
coords.5m<- xyFromCell(run5m,1:ncell(run5m))

#load cells of interest outside the low res flooded cells
load("data/destInds10mat5m.RData")
coords.5mDest<- coords.5m[destInds10m,]

#get the actual flood height at each destination location
trueDestFloodHeights<- c(as.matrix(extract(run5m,coords.5mDest)))

#load mean predictions at each cell
load("data/shiftbyelevdnsclMeanAtDests_10mto5mElev_QGIS.RData")


#Now see if the mean lies with the center 95% of the distribution

#load the estimated variance from comparing the downscaled projs to the HWMs
load("data/varResHWM10mto5m.RData")

#load the mean from the nearest downscaled flooded cell
load("data/shiftbyelevdnsclFromSourceToDest10mto5m_QGIS.RData")

#what's the maximum number that should be plugged into the CDF?
upper<- max(meanFromSourceToDest)+ 3*sqrt(varResHWM10m)

valsToTest<- seq(0,round(upper+.01,2),by=.01)

load("data/shiftbyelevdnsclCDFmat_10mto5mElev_QGIS.RData")

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
mean(inCI95Bds) #0.9990821


################################################################################

#get MAE for dry cells

#look at overall performance
#MAE
mean(abs(meanAtDests-trueDestFloodHeights)) #0.009943803

#MSE
mean((meanAtDests-trueDestFloodHeights)^2) #0.001208797

################################################################################

#total % within 95% PI bounds

PI95accuracy<- (sum(inCI95Bds)+ sum(isBtwn5mby10m))/(length(inCI95Bds)+ length(isBtwn5mby10m))
print(paste0("95% PI coverage: ", PI95accuracy)) #0.9797066

MAE<- (sum(abs(meanAtDests-trueDestFloodHeights)) + sum(abs(downscale10m-floodvals5mby10m)))/(length(meanAtDests) + length(downscale10m))
print(paste0("MAE: ", MAE)) #0.1325829


save(PI95accuracy,MAE,file="data/PI95accuracyMAE.RData")
