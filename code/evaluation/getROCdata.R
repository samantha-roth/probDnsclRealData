#compute ROC curve and AUC

rm(list=ls())

library(terra)
setwd("/Users/f007f8t/Documents/GitHub/probDnsclRealData")

################################################################################
#load estimated variance
load("data/varResHWM10mto5m.RData")

#load flooded locations at the two lower resolutions being downscaled
load("data/floodInds10mat5mAroundHWMs.RData")

#load region of interest coordinates
load("data/boxAroundHWMs.5m.RData")

#load calibrated 5m flood projections in region of interest
load("data/vals5minBdsAroundHWMs.RData")

#load downscaled calibrated flood projections in region of interest
load("data/downscale10mto5mAroundHWMs.RData")

#define the vector holding thresholds to be used in the ROC curve
threshold<- seq(0.01,1,by=0.01)
################################################################################

floodvals5mby10m<- vals5minBds[floodInds10mat5m]

################################################################################
################################################################################
#performance on high resolution cells within wet low resolution cells
################################################################################
################################################################################

predNoFloodInds_WetLowRes<- matrix(NA, nrow= length(threshold), ncol= length(floodInds10mat5m))

pNoFlood<- rep(NA, length(downscale10m))
for(i in 1:length(downscale10m)){
  pNoFlood[i]<- pnorm(0.3, mean= downscale10m[i], sd = sqrt(varResHWM10m))
}

noFlood5mInds_WetLowRes<- ifelse(floodvals5mby10m<=0.3,1,0)

for(t in 1:length(threshold)){
  predNoFloodInds_WetLowRes[t,]<- ifelse(pNoFlood>=threshold[t],1,0)
}

whichNoFlood5mInds_WetLowRes<- which(floodvals5mby10m<=0.3)
whichFlood5mInds_WetLowRes<- which(floodvals5mby10m>0.3)
################################################################################
################################################################################
#now consider high resolution cells within dry low resolution cells
################################################################################
################################################################################

run5m<- rast("data/Outputs5m/Run_1.asc")
load("data/coords.5m.RData")

#load cells of interest outside the low res flooded cells
load("data/destInds10mat5m.RData")
coords.5mDest<- coords.5m[destInds10m,]


#Now bring in the probability of being in the mapped-to distribution
load("data/modelProbFloodbyElev/predProbFloodatDest_5mElev10mModel.RData")
predProbFlood5mElev<- c(predProbFlood)

################################################################################

##If using the downscaled WSE shifted by the elevation at the destination

##load the mean from the nearest downscaled flooded cell
load("data/shiftbyelevdnsclFromSourceToDest10mto5m_QGIS.RData")

################################################################################
probleq.3GivenNotPtMass<- rep(NA,length(meanFromSourceToDest))
totProbleq.3<- rep(NA,length(meanFromSourceToDest))

#load the estimated variance from comparing the downscaled projs to the HWMs
load("data/varResHWM10mto5m.RData")

for(i in 1:length(meanFromSourceToDest)){
  probleq.3GivenNotPtMass[i]<- pnorm(0.3, mean = meanFromSourceToDest[i], sd = sqrt(varResHWM10m))
  totProbleq.3[i]<- (1-predProbFlood5mElev[i])+predProbFlood5mElev[i]*probleq.3GivenNotPtMass[i]
}

################################################################################
#PERFORMANCE FOR DRY HIGH RESOLUTION CELLS WITHIN DRY LOW RESOLUTION CELLS

#what percent of not flooded cells were correctly identified?

#get the actual flood height at each destination location
trueDestFloodHeights<- c(as.matrix(extract(run5m,coords.5mDest)))

noFlood5mInds_DryLowRes<- ifelse(trueDestFloodHeights<=0.3,1,0)

predNoFloodInds_DryLowRes<- matrix(NA, nrow= length(threshold), ncol= length(trueDestFloodHeights))

for(t in 1:length(threshold)){
  predNoFloodInds_DryLowRes[t,]<- ifelse(totProbleq.3>=threshold[t],1,0)
}


whichNoFlood5mInds_DryLowRes<- which(trueDestFloodHeights<=0.3)
whichFlood5mInds_DryLowRes<- which(trueDestFloodHeights>0.3)
################################################################################
################################################################################
########################### get TPR and FPR ####################################
################################################################################
################################################################################

#counting flooded as positive and not flooded as negative, need TPR = TP/P and FPR = 1- TN/N

TPRbyThreshold<- rep(NA,length(threshold))
TNRbyThreshold<- rep(NA,length(threshold))

for(t in 1:length(threshold)){
  predNoFloodInds_DryLowRes_t<- which(predNoFloodInds_DryLowRes[t,]==1)
  predFloodInds_DryLowRes_t<- which(predNoFloodInds_DryLowRes[t,]==0)
  
  predNoFloodInds_WetLowRes_t<- which(predNoFloodInds_WetLowRes[t,]==1)
  predFloodInds_WetLowRes_t<- which(predNoFloodInds_WetLowRes[t,]==0)
  
  
  TPR_WetLowRes<- length(intersect(predFloodInds_WetLowRes_t, whichFlood5mInds_WetLowRes))/length(whichFlood5mInds_WetLowRes)
  TPR_DryLowRes<- length(intersect(predFloodInds_DryLowRes_t, whichFlood5mInds_DryLowRes))/length(whichFlood5mInds_DryLowRes)
  
  TNR_WetLowRes<- length(intersect(predNoFloodInds_WetLowRes_t, whichNoFlood5mInds_WetLowRes))/length(whichNoFlood5mInds_WetLowRes)
  TNR_DryLowRes<- length(intersect(predNoFloodInds_DryLowRes_t, whichNoFlood5mInds_DryLowRes))/length(whichNoFlood5mInds_DryLowRes)
  
  TPR_all<- (TPR_WetLowRes*length(whichFlood5mInds_WetLowRes) + TPR_DryLowRes*length(whichFlood5mInds_DryLowRes))/(length(whichFlood5mInds_WetLowRes) + length(whichFlood5mInds_DryLowRes))
  
  TNR_all<- (TNR_WetLowRes*length(whichNoFlood5mInds_WetLowRes) + TNR_DryLowRes*length(whichNoFlood5mInds_DryLowRes))/(length(whichNoFlood5mInds_WetLowRes) + length(whichNoFlood5mInds_DryLowRes))
  
  TPRbyThreshold[t]<- TPR_all
  TNRbyThreshold[t]<- TNR_all
}

FPRbyThreshold<- 1-TNRbyThreshold

ROC_data<- data.frame("TPR"= TPRbyThreshold,"FPR"= FPRbyThreshold)

save(ROC_data,file="data/ROC_data")