#compute ROC data for other flood events

rm(list=ls())

library(terra)
library(crch)

setwd("/Users/f007f8t/probDnsclRealData_truncatedT")

n_obs=5

#load estimated variance
load("data/varResHWM10mto5m.RData")

load("data/coords.5m.RData")

#load region of interest coordinates
load("data/boxAroundHWMs.5m.RData")

flood<- c("flood2014","flood2020","floodfuture")

threshold<- seq(0.001,1,by=0.001)

for(f in 1:length(flood)){
  print(flood[f])
  
  ################################################################################
  #load flooded locations at the two lower resolutions being downscaled
  load(paste0("data/",flood[f],"/floodInds10mat5mAroundHWMs.RData"))
  
  #load calibrated 5m flood projections in region of interest
  load(paste0("data/",flood[f],"/vals5minBdsAroundHWMs.RData"))
  
  #load downscaled calibrated flood projections in region of interest
  load(paste0("data/",flood[f],"/downscale10mto5mAroundHWMs.RData"))
  ################################################################################
  
  floodvals5mby10m<- vals5minBds[floodInds10mat5m]
  
  ################################################################################
  #performance on high resolution cells within wet low resolution cells
  ################################################################################
  
  predFloodInds_WetLowRes<- matrix(NA, nrow= length(threshold), ncol= length(floodInds10mat5m))
  
  pFlood<- rep(NA, length(downscale10m))
  for(i in 1:length(downscale10m)){
    # pFlood[i]<- 1-pt((0.3-downscale10m[i])/(sqrt(varResHWM10m)),n_obs-1)
    pFlood[i]<- 1-ptt(0.3, location = downscale10m[i], scale = sqrt(varResHWM10m), df = n_obs-1, left = 0, right = Inf)
  }
  
  flood5mInds_WetLowRes<- ifelse(floodvals5mby10m>0.3,1,0)
  
  for(t in 1:length(threshold)){
    predFloodInds_WetLowRes[t,]<- ifelse(pFlood>=threshold[t],1,0)
  }
  
  whichNoFlood5mInds_WetLowRes<- which(floodvals5mby10m<=0.3)
  whichFlood5mInds_WetLowRes<- which(floodvals5mby10m>0.3)
  
  ################################################################################
  #now consider high resolution cells within dry low resolution cells
  ################################################################################
  
  run5m<- rast(paste0("data/Outputs5m/",flood[f],"/Run_1.asc"))

  #load cells of interest outside the low res flooded cells
  load(paste0("data/",flood[f],"/destInds10mat5m.RData"))
  coords.5mDest<- coords.5m[destInds10m,]
  
  
  #Now bring in the probability of being in the mapped-to distribution
  load(paste0("data/",flood[f],"/modelProbFloodbyElev/predProbFloodatDest_5mElev10mModel.RData"))
  predProbFlood5mElev<- c(predProbFlood)
  
  ################################################################################
  
  ##If using the downscaled WSE shifted by the elevation at the destination
  
  ##load the mean from the nearest downscaled flooded cell
  load(paste0("data/",flood[f],"/shiftbyelevdnsclFromSourceToDest10mto5m_QGIS.RData"))
  
  ################################################################################
  probleq.3GivenNotPtMass<- rep(NA,length(meanFromSourceToDest))
  totProbleq.3<- rep(NA,length(meanFromSourceToDest))
  totProbg.3<- rep(NA,length(meanFromSourceToDest))
  
  for(i in 1:length(meanFromSourceToDest)){
    # probleq.3GivenNotPtMass[i]<- pt((0.3-meanFromSourceToDest[i])/(sqrt(varResHWM10m)),n_obs-1)
    probleq.3GivenNotPtMass[i]<- ptt(0.3, location = meanFromSourceToDest[i], scale = sqrt(varResHWM10m), df = n_obs-1, left = 0, right = Inf)
    totProbleq.3[i]<- (1-predProbFlood5mElev[i])+predProbFlood5mElev[i]*probleq.3GivenNotPtMass[i]
    totProbg.3[i]<- 1-totProbleq.3[i]
  }
  
  ################################################################################
  #PERFORMANCE FOR DRY HIGH RESOLUTION CELLS WITHIN DRY LOW RESOLUTION CELLS
  
  #what percent of not flooded cells were correctly identified?
  
  #get the actual flood height at each destination location
  trueDestFloodHeights<- c(as.matrix(extract(run5m,coords.5mDest)))
  
  flood5mInds_DryLowRes<- ifelse(trueDestFloodHeights>0.3,1,0)
  
  predFloodInds_DryLowRes<- matrix(NA, nrow= length(threshold), ncol= length(trueDestFloodHeights))
  
  for(t in 1:length(threshold)){
    predFloodInds_DryLowRes[t,]<- ifelse(totProbg.3>=threshold[t],1,0)
  }
  
  
  whichNoFlood5mInds_DryLowRes<- which(trueDestFloodHeights<=0.3)
  whichFlood5mInds_DryLowRes<- which(trueDestFloodHeights>0.3)

  ################################################################################
  ########################### get TPR and FPR ####################################
  ################################################################################
  
  #counting flooded as positive and not flooded as negative, need TPR = TP/P and FPR = 1- TN/N
  
  TPRbyThreshold<- rep(NA,length(threshold))
  TNRbyThreshold<- rep(NA,length(threshold))
  
  for(t in 1:length(threshold)){
    predNoFloodInds_DryLowRes_t<- which(predFloodInds_DryLowRes[t,]==0)
    predFloodInds_DryLowRes_t<- which(predFloodInds_DryLowRes[t,]==1)
    
    predNoFloodInds_WetLowRes_t<- which(predFloodInds_WetLowRes[t,]==0)
    predFloodInds_WetLowRes_t<- which(predFloodInds_WetLowRes[t,]==1)
    
    
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
  FNRbyThreshold<- 1-TPRbyThreshold
  
  ROC_data<- data.frame("TPR"= TPRbyThreshold,"FPR"= FPRbyThreshold, "TNR"= TNRbyThreshold, "FNR"=FNRbyThreshold)
  
  save(ROC_data,file=paste0("data/",flood[f],"/ROC_data"))
  
}