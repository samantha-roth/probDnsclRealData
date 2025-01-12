
rm(list=ls())

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

flood<- c("flood2014","flood2020","floodfuture")

for(f in 1:length(flood)){
  
  print(flood[f])
  
  load(paste0("data/",flood[f],"/wetCellAccuracy_.3mflood_10mto5m.RData"))
  
  nWetFlood<- nFlood
  nWetNoFlood<- nNoFlood
  
  nSens_LRW<- sens_LRW*nFlood
  nSpec_LRW<- spec_LRW*nNoFlood
  
  load(paste0("data/",flood[f],"/estProb.3mFloodIndsofInterest_10mto5mElev_QGIS.RData"))
  
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
  
  save(totalSens,totalSpec,totAcc,file=paste0("data/",flood[f],"/SensSpecAccuracy_.3mflood.RData"))
}

# "flood2014"
# "sensitivity: 0.945243128964059"
# "specificity: 0.998457339503975"
# "total accuracy: 0.979326594208406"
# "flood2020"
# "sensitivity: 0.953155926984825"
# "specificity: 0.999419279907085"
# "total accuracy: 0.983430873299384"
# "floodfuture"
# "sensitivity: 0.971943887775551"
# "specificity: 0.997860504920839"
# "total accuracy: 0.981150718248841"
