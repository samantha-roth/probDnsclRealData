#Get the probability of each destination having a flood height =0 

rm(list=ls())

library(terra)

setwd("C:/Users/svr5482")

flow<- c("Q2559.8429","Q2503.2092","Q3681.19006")

for(f in 1:length(flow)){
  print(flow[f])
  
  pt<-proc.time()
  
  run5m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs5m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  coords.5m<- xyFromCell(run5m,1:ncell(run5m))
  
  #load cells of interest outside the low res flooded cells
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/destInds30mat5m.RData"))
  coords.5mDest<- coords.5m[destInds30m,]
  
  
  #Now bring in the probability of being in the mapped-to distribution
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/modelProbFloodbyElev/predProbFloodatDest_5mElev30mModel.RData"))
  predProbFlood5mElev<- c(predProbFlood)
  
  #load("probabilisticDownscaling/data/simObs/modelProbFloodbyCost/predProbFloodatDest_30mCost30mModel.RData")
  #predProbFlood30mCost<- c(predProbFlood)
  
  ################################################################################
  
  ##If using the downscaled WSE shifted by the elevation at the destination
  
  ##load the mean from the nearest downscaled flooded cell
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/shiftbyelevdnsclFromSourceToDest30mto5m.RData"))
  
  ################################################################################
  ##If using the unshifted downscaled version
  
  ##load the estimated mean from the nearest downscaled flooded cell
  #load("probabilisticDownscaling/data/simObs/dnsclFromSourceToDest30m.RData")
  
  ################################################################################
  probleq0GivenNotPtMass<- rep(NA,length(meanFromSourceToDest))
  totProbleq0<- rep(NA,length(meanFromSourceToDest))
  
  #load the estimated variance from comparing the downscaled projs to the HWMs
  load("probabilisticDownscaling/data/simObs/varResHWM30mto5m.RData")
  
  for(i in 1:length(meanFromSourceToDest)){
    probleq0GivenNotPtMass[i]<- pnorm(0, mean = meanFromSourceToDest[i], sd = sqrt(varResHWM30m))
    totProbleq0[i]<- (1-predProbFlood5mElev[i])+predProbFlood5mElev[i]*probleq0GivenNotPtMass[i]
  }
  
  ptFinal<-proc.time()-pt
  time_estProbFloodIndsofInterest<-ptFinal[3]
  save(time_estProbFloodIndsofInterest, 
       file= paste0("probabilisticDownscaling/data/",flow[f],"/simObs/time_estProbFloodIndsofInterest_30mto5mElev.RData"))
  
  ################################################################################
  #PERFORMANCE FOR DRY CELLS
  #what percent of not flooded cells were correctly identified?
  
  #get the actual flood height at each destination location
  trueDestFloodHeights<- c(as.matrix(extract(run5m,coords.5mDest)))
  
  
  indsProbNotFloodgeq.5<- which(totProbleq0>=.5) 
  lenProbNotFloodgeq.5<- length(which(totProbleq0>=.5)) #Am
  
  indsNotFlood<- which(trueDestFloodHeights==0)
  indsNotFloodCorrect<- which(indsProbNotFloodgeq.5%in%indsNotFlood)
  
  lenNotFlood<- length(which(trueDestFloodHeights==0)) #Ar
  lenNotFloodCorrect<- length(which(indsProbNotFloodgeq.5%in%indsNotFlood)) #Arm
  
  #CORRECTNESS FOR DRY CELLS #(A and B) / B
  print(paste0("specificity: ",lenNotFloodCorrect/lenNotFlood))
  
  #downscaled and shifted by elevation
  #1: all dry cells are identified
  
  #FIT FOR DRY CELLS #(A and B) / (A + B - A and B)
  lenNotFloodCorrect/(lenProbNotFloodgeq.5 + lenNotFlood -lenNotFloodCorrect)
  
  #downscaled and shifted by elevation
  #0.9027067 #compared to the number of cells correctly predicted to be dry,
  #the number of additional cells predicted to be dry is small
  
  ################################################################################
  #PERFORMANCE FOR FLOODED CELLS
  #what percent of flooded cells were correctly identified
  indsProbFloodgeq.5<- which(totProbleq0<.5)
  indsFlood<- which(trueDestFloodHeights>0)
  indsFloodCorrect<- which(indsProbFloodgeq.5%in%indsFlood)
  
  lenProbFloodgeq.5<- length(which(totProbleq0<.5)) #Am
  lenFlood<- length(which(trueDestFloodHeights>0)) #Ar
  lenFloodCorrect<- length(which(indsProbFloodgeq.5%in%indsFlood)) #Arm
  
  #CORRECTNESS FOR FLOODED CELLS #(A and B) / B
  print(paste0("sensitivity: ",lenFloodCorrect/lenFlood))
  
  #downscaled and shifted by elevation:
  #0.4884615 (5m costs) of wet destination cells were identified as 
  #having >50% chance of being flooded
  
  #FIT FOR DRY CELLS #(A and B) / (A + B - A and B)
  lenFloodCorrect/(lenProbFloodgeq.5 + lenFlood -lenFloodCorrect)
  
  #downscaled and shifted by elevation
  #0.4884615= correctness, so all of the cells that were predicted to be flooded were
  #in the right place, but only half of them were identified
  ################################################################################
  
  #TOTAL ACCURACY FOR BOTH WET AND DRY CELLS
  
  print(paste0("total accuracy: ",(lenFloodCorrect+lenNotFloodCorrect)/(lenProbFloodgeq.5+lenProbNotFloodgeq.5)))
  
  #downscaled and shifted by elevation
  #0.9109772 that's pretty damn good
  
  ################################################################################
  
  #save indices of interest
  indsNotFloodCorrect<- indsProbNotFloodgeq.5[which(indsProbNotFloodgeq.5%in%indsNotFlood)]
  indsFalseNeg<- indsProbNotFloodgeq.5[which(indsProbNotFloodgeq.5%in%setdiff(indsProbNotFloodgeq.5,indsNotFlood))]
  
  indsFloodCorrect<- indsProbFloodgeq.5[which(indsProbFloodgeq.5%in%indsFlood)]
  indsFalsePos<- indsProbFloodgeq.5[which(indsProbFloodgeq.5%in%setdiff(indsProbFloodgeq.5,indsFlood))]
  
  save(indsProbNotFloodgeq.5,indsProbFloodgeq.5,
       indsNotFloodCorrect,indsFloodCorrect,
       indsFalseNeg,indsFalsePos,lenFlood,lenNotFlood, indsFlood, indsNotFlood,
       file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/estProbFloodIndsofInterest_30mto5mElev.RData"))
  
  ################################################################################
  pFloodDry<- 1-totProbleq0
  save(pFloodDry,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/pFloodDry_30mto5mElev.RData"))
  
}

# "Q2559.8429"
# "specificity: 0.984771573604061"
# "sensitivity: 0.502564102564103"
# "total accuracy: 0.941339491916859"
# "Q2503.2092"
# "specificity: 0.98901623686724"
# "sensitivity: 0.469387755102041"
# "total accuracy: 0.934587430525866"
# "Q3681.19006"
# "specificity: 1"
# "sensitivity: 0.247863247863248"
# "total accuracy: 0.91295746785361"