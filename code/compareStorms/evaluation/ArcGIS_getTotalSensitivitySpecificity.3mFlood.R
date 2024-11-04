
rm(list=ls())

setwd("C:/Users/svr5482")

flood<- c("flood2014","flood2020","floodfuture")

for(f in 1:length(flood)){
  
  print(flood[f])
  
  load(paste0("probDnsclRealData/data/",flood[f],"/wetCellAccuracy_.3mflood_10mto5m.RData"))
  
  nWetFlood<- nFlood
  nWetNoFlood<- nNoFlood
  
  nSens_LRW<- sens_LRW*nFlood
  nSpec_LRW<- spec_LRW*nNoFlood
  
  load(paste0("probDnsclRealData/data/",flood[f],"/estProb.3mFloodIndsofInterest_10mto5mElev.RData"))
  
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
  
  totAcc<- (nSens_LRW + nSens_LRD + nSpec_LRW + nSpec_LRD)/(nWetFlood+nDryFlood + nWetNoFlood+nDryNoFlood)
  print(paste0("total accuracy: ",totAcc))
}

# "flood2014"
# "sensitivity: 0.93347755543537"
# "specificity: 0.995400788436268"
# "flood2020"
# "sensitivity: 0.940676365010391"
# "specificity: 0.997711088504578"
# "floodfuture"
# "sensitivity: 0.974527431996312"
# "specificity: 0.998437848694488"
