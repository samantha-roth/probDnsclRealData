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
  ##If using the unshifted downscaled version
  
  ##load the estimated mean from the nearest downscaled flooded cell
  #load("data/dnsclFromSourceToDest10m.RData")
  
  ################################################################################
  probleq0GivenNotPtMass<- rep(NA,length(meanFromSourceToDest))
  totProbleq0<- rep(NA,length(meanFromSourceToDest))
  
  #load the estimated variance from comparing the downscaled projs to the HWMs
  load("data/varResHWM10mto5m.RData")
  
  for(i in 1:length(meanFromSourceToDest)){
    probleq0GivenNotPtMass[i]<- pnorm(0, mean = meanFromSourceToDest[i], sd = sqrt(varResHWM10m))
    totProbleq0[i]<- (1-predProbFlood5mElev[i])+predProbFlood5mElev[i]*probleq0GivenNotPtMass[i]
  }
  
  ptFinal<-proc.time()-pt
  time_estProbFloodIndsofInterest<-ptFinal[3]
  save(time_estProbFloodIndsofInterest, 
       file= paste0("data/",flood[f],"/time_estProbFloodIndsofInterest_10mto5mElev_QGIS.RData"))
  
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
  
  #FIT FOR DRY CELLS #(A and B) / (A + B - A and B)
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
       file=paste0("data/",flood[f],"/estProbFloodIndsofInterest_10mto5mElev_QGIS.RData"))
  
  ################################################################################
  pFloodDry<- 1-totProbleq0
  save(pFloodDry,file=paste0("data/",flood[f],"/pFloodDry_10mto5mElev_QGIS.RData"))
  
}

# "flood2014"
# "specificity: 0.997334754797441"
# "sensitivity: 0.366233766233766"
# "total accuracy: 0.96653568259602"
# "flood2020"
# "specificity: 0.998842741416999"
# "sensitivity: 0.381818181818182"
# "total accuracy: 0.973726409275934"
# "floodfuture"
# "specificity: 0.999551569506726"
# "sensitivity: 0.447154471544715"
# "total accuracy: 0.984726161902684"