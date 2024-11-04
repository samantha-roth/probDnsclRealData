
rm(list=ls())

setwd("C:/Users/svr5482")

flow<- c("Q2559.8429","Q2503.2092","Q3681.19006")

for(f in 1:length(flow)){
  
  print(flow[f])
  
  load(paste0("probabilisticDownscaling/data/",flow[f],"/wetCellAccuracy30mto5m.RData"))
  
  nWetFlood<- nFlood
  nWetNoFlood<- nNoFlood
  
  nSens_LRW<- sens_LRW*nFlood
  nSpec_LRW<- spec_LRW*nNoFlood
  
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/estProbFloodIndsofInterest_30mto5mElev.RData"))
  
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

# "Q2559.8429"
# "sensitivity: 0.900445765230312"
# "specificity: 0.965947242206235"
# "Q2503.2092"
# "sensitivity: 0.891933815925543"
# "specificity: 0.978341013824885"
# "Q3681.19006"
# "sensitivity: 0.929626138862708"
# "specificity: 1"
