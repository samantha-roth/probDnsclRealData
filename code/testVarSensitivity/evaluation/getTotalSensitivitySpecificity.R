
rm(list=ls())

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

#load the sampled variances from the empirical distribution 
load("data/var_samples")

for(v in 1: length(var_samples)){
  
  varResHWM10m<- var_samples[v]
  
  print(paste0("var sample= ", var_samples[v]))
  
  load(paste0("data/var_sample",v,"/wetCellAccuracy10mto5m.RData"))
  
  nWetFlood<- nFlood
  nWetNoFlood<- nNoFlood
  
  nSens_LRW<- sens_LRW*nFlood
  nSpec_LRW<- spec_LRW*nNoFlood
  
  load(paste0("data/var_sample",v,"/estProbFloodIndsofInterest_10mto5mElev_QGIS.RData"))
  
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
}
