
rm(list=ls())

setwd("C:/Users/svr5482")

flow<- c("Q2559.8429","Q2503.2092","Q3681.19006")

for(f in 1:length(flow)){
  
  print(flow[f])
  
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/interpVarDnsclErrbyMeanAbsElevDiff/wetCellAccuracy10mto5m.RData"))
  
  nWetFlood<- nFlood
  nWetNoFlood<- nNoFlood
  
  nSens_LRW<- sens_LRW*nFlood
  nSpec_LRW<- spec_LRW*nNoFlood
  
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/interpVarDnsclErrbyMeanAbsElevDiff/estProbFloodIndsofInterest_10mto5m.RData"))
  
  nDryFlood<- lenFlood
  nDryNoFlood<- lenNotFlood
  
  nSens_LRD<- length(indsFloodCorrect)
  nSpec_LRD<- length(indsNotFloodCorrect)
  
  totalSens<- (nSens_LRW + nSens_LRD)/(nWetFlood+nDryFlood)
  print(paste0("sensitivity: ", totalSens)) #0.9326279
  totalSpec<- (nSpec_LRW + nSpec_LRD)/(nWetNoFlood+nDryNoFlood)
  print(paste0("specificity: ", totalSpec)) #0.9897557
  
  
  nDryFlood + nDryNoFlood
  
  nWetFlood + nWetNoFlood
}

#[1] "Q2559.8429"
#[1] "sensitivity: 0.975730559683011"
#[1] "specificity: 0.993285371702638"
#[1] "Q2503.2092"
#[1] "sensitivity: 0.977249224405377"
#[1] "specificity: 0.995852534562212"
#[1] "Q3681.19006"
#[1] "sensitivity: 0.995601633678919"
#[1] "specificity: 0.992399565689468"

