
rm(list=ls())

setwd("C:/Users/svr5482")

load("probDnsclRealData/data/wetCellAccuracy_.3mflood_10mto5m.RData")

nWetFlood<- nFlood
nWetNoFlood<- nNoFlood

nSens_LRW<- sens_LRW*nFlood
nSpec_LRW<- spec_LRW*nNoFlood

load("probDnsclRealData/data/estProb.3mFloodIndsofInterest_10mto5mElev.RData")

nDryFlood<- lenFlood
nDryNoFlood<- lenNotFlood

nSens_LRD<- length(indsFloodCorrect)
nSpec_LRD<- length(indsNotFloodCorrect)

totalSens<- (nSens_LRW + nSens_LRD)/(nWetFlood+nDryFlood)
totalSens #0.9339814
totalSpec<- (nSpec_LRW + nSpec_LRD)/(nWetNoFlood+nDryNoFlood)
totalSpec #0.9993043

accuracy<- (nSens_LRW + nSens_LRD + nSpec_LRW + nSpec_LRD)/(nWetFlood+nDryFlood + nWetNoFlood+nDryNoFlood)
accuracy #0.9625295