#Get the probability of each destination having a flood height =0 

rm(list=ls())

library(terra)

setwd("C:/Users/svr5482")

pt<-proc.time()

run5m<- rast("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs5m/Norristown/nCh/postMedCh/Extent/Run_1.asc")
load("probDnsclRealData/data/coords.5m.RData")

#load cells of interest outside the low res flooded cells
load("probDnsclRealData/data/destInds10mat5m.RData")
coords.5mDest<- coords.5m[destInds10m,]


#Now bring in the probability of being in the mapped-to distribution
load("probDnsclRealData/data/modelProbFloodbyElev/predProbFloodatDest_5mElev10mModel.RData")
predProbFlood5mElev<- c(predProbFlood)

################################################################################

##If using the downscaled WSE shifted by the elevation at the destination

##load the mean from the nearest downscaled flooded cell
load("probDnsclRealData/data/shiftbyelevdnsclFromSourceToDest10mto5m.RData")

################################################################################
##If using the unshifted downscaled version

##load the estimated mean from the nearest downscaled flooded cell
#load("probDnsclRealData/data/dnsclFromSourceToDest10m.RData")

################################################################################
probleq0GivenNotPtMass<- rep(NA,length(meanFromSourceToDest))
totProbleq0<- rep(NA,length(meanFromSourceToDest))

#load the estimated variance from comparing the downscaled projs to the HWMs
load("probDnsclRealData/data/varResHWM10mto5m.RData")

for(i in 1:length(meanFromSourceToDest)){
  probleq0GivenNotPtMass[i]<- pnorm(0, mean = meanFromSourceToDest[i], sd = sqrt(varResHWM10m))
  totProbleq0[i]<- (1-predProbFlood5mElev[i])+predProbFlood5mElev[i]*probleq0GivenNotPtMass[i]
}

ptFinal<-proc.time()-pt
time_estProbFloodIndsofInterest<-ptFinal[3]
save(time_estProbFloodIndsofInterest, file= "probDnsclRealData/data/time_estProbFloodIndsofInterest_10mto5mElev.RData")

################################################################################

##If using the shifted downscaled version

##load the estimated mean from the nearest downscaled flooded cell
#load("probDnsclRealData/data/shiftdnsclFromSourceToDest10m.RData")

#probleq0GivenNotPtMass<- rep(NA,length(meanFromSourceToDest))
#totProbleq0<- rep(NA,length(meanFromSourceToDest))

##load the estimated variance from comparing the downscaled projs to the HWMs
#load("probDnsclRealData/data/varShiftResHWM10m.RData")

#for(i in 1:length(meanFromSourceToDest)){
#  probleq0GivenNotPtMass[i]<- pnorm(0, mean = meanFromSourceToDest[i], sd = sqrt(varShiftResHWM10m))
#  totProbleq0[i]<- (1-predProbFlood5mElev[i])+predProbFlood5mElev[i]*probleq0GivenNotPtMass[i]
#}

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
lenNotFloodCorrect/lenNotFlood 
#downscaled and shifted by elevation
#0.9996177: almost all dry cells are identified

#FIT FOR DRY CELLS #(A and B) / (A + B - A and B)
lenNotFloodCorrect/(lenProbNotFloodgeq.5 + lenNotFlood -lenNotFloodCorrect)
#downscaled and shifted by elevation
#0.9737479 #compared to the number of cells correctly predicted to be dry,
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
lenFloodCorrect/lenFlood

#downscaled and shifted by elevation
#0.3534884: about 2/3 dry cells are identified

#FIT FOR FLOODED CELLS #(A and B) / (A + B - A and B)
lenFloodCorrect/(lenProbFloodgeq.5 + lenFlood -lenFloodCorrect)
#downscaled and shifted by elevation
#0.3502304= fit
################################################################################

#TOTAL ACCURACY FOR BOTH WET AND DRY CELLS

(lenFloodCorrect+lenNotFloodCorrect)/(lenProbFloodgeq.5+lenProbNotFloodgeq.5)

#downscaled and shifted by elevation
#0.9741142 that's pretty damn good

################################################################################

#save indices of interest
indsNotFloodCorrect<- indsProbNotFloodgeq.5[which(indsProbNotFloodgeq.5%in%indsNotFlood)]
indsFalseNeg<- indsProbNotFloodgeq.5[which(indsProbNotFloodgeq.5%in%setdiff(indsProbNotFloodgeq.5,indsNotFlood))]

indsFloodCorrect<- indsProbFloodgeq.5[which(indsProbFloodgeq.5%in%indsFlood)]
indsFalsePos<- indsProbFloodgeq.5[which(indsProbFloodgeq.5%in%setdiff(indsProbFloodgeq.5,indsFlood))]

save(indsProbNotFloodgeq.5,indsProbFloodgeq.5,
     indsNotFloodCorrect,indsFloodCorrect,
     indsFalseNeg,indsFalsePos,lenFlood,lenNotFlood, indsFlood, indsNotFlood,
     file="probDnsclRealData/data/estProbFloodIndsofInterest_10mto5mElev.RData")

################################################################################
pFloodDry<- 1-totProbleq0
save(pFloodDry,file="probDnsclRealData/data/pFloodDry_10mto5mElev.RData")
