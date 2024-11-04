#Get the probability of each destination having a flood height =0 

rm(list=ls())

library(terra)

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

flood<- c("flood2014","flood2020","floodfuture")

for(f in 1:length(flood)){
  print(flood[f])
  
  pt<-proc.time()
  
  run5m<- rast(paste0("data/Outputs5m/",flood[f],"/Run_1.asc"))
  coords.5m<- xyFromCell(run5m,1:ncell(run5m))
  
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
  
  #load the estimated variance from comparing the downscaled projs to the HWMs
  load("data/varResHWM10mto5m.RData")
  
  for(i in 1:length(meanFromSourceToDest)){
    probleq.3GivenNotPtMass[i]<- pnorm(0.3, mean = meanFromSourceToDest[i], sd = sqrt(varResHWM10m))
    totProbleq.3[i]<- (1-predProbFlood5mElev[i])+predProbFlood5mElev[i]*probleq.3GivenNotPtMass[i]
  }
  
  ptFinal<-proc.time()-pt
  time_estProbFloodIndsofInterest<-ptFinal[3]
  save(time_estProbFloodIndsofInterest, 
       file= paste0("data/",flood[f],"/time_estProbFlood.3mIndsofInterest_10mto5mElev_QGIS.RData"))
  
  ################################################################################
  #PERFORMANCE FOR DRY CELLS
  #what percent of not flooded cells were correctly identified?
  
  #get the actual flood height at each destination location
  trueDestFloodHeights<- c(as.matrix(extract(run5m,coords.5mDest)))
  
  
  indsProbNotFloodgeq.5<- which(totProbleq.3>=.5) 
  lenProbNotFloodgeq.5<- length(which(totProbleq.3>=.5)) #Am
  
  indsNotFlood<- which(trueDestFloodHeights<=0.3)
  indsNotFloodCorrect<- which(indsProbNotFloodgeq.5%in%indsNotFlood)
  
  lenNotFlood<- length(which(trueDestFloodHeights<=0.3)) #Ar
  lenNotFloodCorrect<- length(which(indsProbNotFloodgeq.5%in%indsNotFlood)) #Arm
  
  #CORRECTNESS FOR DRY CELLS #(A and B) / B
  lenNotFloodCorrect/lenNotFlood 
  
  #FIT FOR DRY CELLS #(A and B) / (A + B - A and B)
  lenNotFloodCorrect/(lenProbNotFloodgeq.5 + lenNotFlood -lenNotFloodCorrect)
  
  ################################################################################
  #PERFORMANCE FOR FLOODED CELLS
  #what percent of flooded cells were correctly identified
  indsProbFloodgeq.5<- which(totProbleq.3<.5)
  indsFlood<- which(trueDestFloodHeights>0.3)
  indsFloodCorrect<- which(indsProbFloodgeq.5%in%indsFlood)
  
  lenProbFloodgeq.5<- length(which(totProbleq.3<.5)) #Am
  lenFlood<- length(which(trueDestFloodHeights>0.3)) #Ar
  lenFloodCorrect<- length(which(indsProbFloodgeq.5%in%indsFlood)) #Arm
  
  #CORRECTNESS FOR FLOODED CELLS #(A and B) / B
  lenFloodCorrect/lenFlood
  
  #FIT FOR FLOODED CELLS #(A and B) / (A + B - A and B)
  lenFloodCorrect/(lenProbFloodgeq.5 + lenFlood -lenFloodCorrect)
  
  ################################################################################
  
  #TOTAL ACCURACY FOR BOTH WET AND DRY CELLS
  
  print(paste0("total accuracy: ",(lenFloodCorrect+lenNotFloodCorrect)/(lenProbFloodgeq.5+lenProbNotFloodgeq.5)))
  
  ################################################################################
  
  #save indices of interest
  indsNotFloodCorrect<- indsProbNotFloodgeq.5[which(indsProbNotFloodgeq.5%in%indsNotFlood)]
  indsFalseNeg<- indsProbNotFloodgeq.5[which(indsProbNotFloodgeq.5%in%setdiff(indsProbNotFloodgeq.5,indsNotFlood))]
  
  indsFloodCorrect<- indsProbFloodgeq.5[which(indsProbFloodgeq.5%in%indsFlood)]
  indsFalsePos<- indsProbFloodgeq.5[which(indsProbFloodgeq.5%in%setdiff(indsProbFloodgeq.5,indsFlood))]
  
  save(indsProbNotFloodgeq.5,indsProbFloodgeq.5,
       indsNotFloodCorrect,indsFloodCorrect,
       indsFalseNeg,indsFalsePos,lenFlood,lenNotFlood, indsFlood, indsNotFlood,
       file=paste0("data/",flood[f],"/estProb.3mFloodIndsofInterest_10mto5mElev_QGIS.RData"))
  
  ################################################################################
  pFloodDry<- 1-totProbleq.3
  save(pFloodDry,file=paste0("data/",flood[f],"/p.3mFloodDry_10mto5mElev_QGIS.RData"))
  
}

# "flood2014"
# "total accuracy: 0.996831030548865"
# "flood2020"
# "total accuracy: 0.997656346367337"
# "floodfuture"
# "total accuracy: 0.997163430067641"