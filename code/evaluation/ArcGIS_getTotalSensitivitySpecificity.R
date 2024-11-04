
rm(list=ls())

setwd("C:/Users/svr5482")

load("probDnsclRealData/data/wetCellAccuracy10mto5m.RData")

nWetFlood<- nFlood
nWetNoFlood<- nNoFlood

nSens_LRW<- sens_LRW*nFlood
nSpec_LRW<- spec_LRW*nNoFlood

load("probDnsclRealData/data/estProbFloodIndsofInterest_10mto5mElev.RData")

nDryFlood<- lenFlood
nDryNoFlood<- lenNotFlood

nSens_LRD<- length(indsFloodCorrect)
nSpec_LRD<- length(indsNotFloodCorrect)

totalSens<- (nSens_LRW + nSens_LRD)/(nWetFlood+nDryFlood)
totalSens #0.9640233
totalSpec<- (nSpec_LRW + nSpec_LRD)/(nWetNoFlood+nDryNoFlood)
totalSpec #0.99848