rm(list=ls())

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

load("data/wetCellAccuracy_.3mflood_10mto5m.RData")

nWetFlood<- nFlood
nWetNoFlood<- nNoFlood

nSens_LRW<- sens_LRW*nFlood
nSpec_LRW<- spec_LRW*nNoFlood

load("data/estProb.3mFloodIndsofInterest_10mto5mElev_QGIS.RData")

nDryFlood<- lenFlood
nDryNoFlood<- lenNotFlood

nSens_LRD<- length(indsFloodCorrect)
nSpec_LRD<- length(indsNotFloodCorrect)

totalSens<- (nSens_LRW + nSens_LRD)/(nWetFlood+nDryFlood)
print(paste0("Our method's total sensitivity for .3m flood: ", totalSens)) #0.9339814
totalSpec<- (nSpec_LRW + nSpec_LRD)/(nWetNoFlood+nDryNoFlood)
print(paste0("Our method's total specificity for .3m flood: ", totalSpec)) #0.9993043

accuracy<- (nSens_LRW + nSens_LRD + nSpec_LRW + nSpec_LRD)/(nWetFlood+nDryFlood + nWetNoFlood+nDryNoFlood)
print(paste0("Our method's total accuracy for .3m flood: ", accuracy)) #0.9624534


save(totalSens,totalSpec,accuracy,file="data/totalSensSpecAccuracy.RData")