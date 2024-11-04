
rm(list=ls())

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

load("data/wetCellAccuracy10mto5m.RData")

nWetFlood<- nFlood
nWetNoFlood<- nNoFlood

nSens_LRW<- sens_LRW*nFlood
nSpec_LRW<- spec_LRW*nNoFlood

load("data/estProbFloodIndsofInterest_10mto5mElev_QGIS.RData")

nDryFlood<- lenFlood
nDryNoFlood<- lenNotFlood

nSens_LRD<- length(indsFloodCorrect)
nSpec_LRD<- length(indsNotFloodCorrect)

totalSens<- (nSens_LRW + nSens_LRD)/(nWetFlood+nDryFlood)
print(paste0("total sensitivity: ", totalSens)) #0.9640233
totalSpec<- (nSpec_LRW + nSpec_LRD)/(nWetNoFlood+nDryNoFlood)
print(paste0("total specificity: ", totalSpec)) #0.99848

accuracy<- (nSens_LRW + nSens_LRD + nSpec_LRW + nSpec_LRD)/(nWetFlood+nDryFlood + nWetNoFlood+nDryNoFlood)
print(paste0("total accuracy: ", accuracy)) 