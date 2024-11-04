#plot distribution of flood heights for a few wet and dry destination cells

rm(list=ls())

library(terra)

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

pt<-proc.time()

run5m<- rast("data/Outputs5m/Run_1.asc")
load("data/coords.5m.RData")

#load cells of interest outside the low res flooded cells
load("data/destInds10mat5m.RData")
coords.5mDest<- coords.5m[destInds10m,]

#get the actual flood height at each destination location
trueDestFloodHeights<- c(as.matrix(extract(run5m,coords.5mDest)))

#Now bring in the probability of being in the mapped-to distribution
load("data/modelProbFloodbyElev/predProbFloodatDest_5mElev10mModel.RData")

predProbFlood5mCost<- c(predProbFlood)

################################################################################

#using the downscaled WSE shifted by the elevation at the destination

#load the mean from the nearest downscaled flooded cell
load("data/shiftbyelevdnsclFromSourceToDest10mto5m_QGIS.RData")

################################################################################
probleq0GivenNotPtMass<- rep(NA,length(meanFromSourceToDest))
totProbleq0<- rep(NA,length(meanFromSourceToDest))

#load the estimated variance from comparing the downscaled projections to the HWMs
load("data/varResHWM10mto5m.RData")

for(i in 1:length(meanFromSourceToDest)){
  probleq0GivenNotPtMass[i]<- pnorm(0, mean = meanFromSourceToDest[i], sd = sqrt(varResHWM10m))
  totProbleq0[i]<- (1-predProbFlood5mCost[i])+predProbFlood5mCost[i]*probleq0GivenNotPtMass[i]
}


#get the CDF function for num >=0
CDFatDestPt<- function(sigma, i, num){
  probleqNumGivenNotPtMass<- pnorm(num, mean = meanFromSourceToDest[i], sd = sqrt(varResHWM10m))
  totProbleqNum<- (1-predProbFlood5mCost[i])+predProbFlood5mCost[i]*probleqNumGivenNotPtMass
  totProbleqNum
}

#what's the maximum number that should be plugged into the CDF?
upper<- max(meanFromSourceToDest)+ 3*sqrt(varResHWM10m)

valsToTest<- seq(0,round(upper+.01,2),by=.01)

CDFmat<- matrix(NA, nrow=length(meanFromSourceToDest), ncol= length(valsToTest))

for(j in 1:length(valsToTest)){
  for(i in 1:length(meanFromSourceToDest)){
    CDFmat[i,j]<- CDFatDestPt(sigma=sqrt(varResHWM10m),i=i,num=valsToTest[j])
  }
}

save(CDFmat,file="data/shiftbyelevdnsclCDFmat_10mto5mElev_QGIS.RData")

approxPDFmat<- matrix(NA, nrow=length(meanFromSourceToDest), ncol= length(valsToTest))

for(j in 1:length(valsToTest)){
  for(i in 1:length(meanFromSourceToDest)){
    if(j==1) approxPDFmat[i,j]<-CDFmat[i,j]
    if(j>1) approxPDFmat[i,j]<-CDFmat[i,j]-CDFmat[i,j-1]
  }
}


save(approxPDFmat,file="data/shiftbyelevdnsclApproxPDFmat_10mto5mElev_QGIS.RData")
#pick some locations (rows) of interest to look at based on performance
load("data/estProbFloodIndsofInterest_10mto5mElev_QGIS.RData")

################################################################################
#get the expectation for each destination cell

meanAtDests<- c(approxPDFmat%*%valsToTest)

mean(meanAtDests[indsProbNotFloodgeq.5]) #0.008706689
mean(meanAtDests[indsProbFloodgeq.5]) #0.2330105

mean(meanAtDests[indsFalseNeg]) #0.06522477
mean(meanAtDests[indsNotFloodCorrect]) #0.007204583

mean(meanAtDests[indsFloodCorrect]) #0.2360047

mean(meanAtDests[c(indsNotFloodCorrect)]) #0.007204583
mean(meanAtDests[c(indsFalseNeg,indsFloodCorrect)]) #0.1255935

save(meanAtDests,file="data/shiftbyelevdnsclMeanAtDests_10mto5mElev_QGIS.RData")

ptFinal<-proc.time()-pt
time_getCDFandPDFatDests<-ptFinal[3]
save(time_getCDFandPDFatDests, file= "data/time_getCDFandPDFatDests_10mto5mElev_QGIS.RData")

################################################################################
#plot distribution of flood heights for a few nonflooded destination cells
##downscaled and shifted by elevation
#set.seed(29)
#sampFN<- sample(indsFalseNeg,1)
#sampTN<- sample(indsNotFloodCorrect,12)
#sampTP<- sample(indsFloodCorrect,1)

#plot(valsToTest,approxPDFmat[sampFN,],type="l",main="False Negative")

#for(i in 1:length(sampTN)){
#  print(plot(x=valsToTest,y=approxPDFmat[sampTN[i],],type="l",main=paste0("True Negative ",i)))
#}


#print(plot(x=valsToTest,y=approxPDFmat[sampTP,],type="l",main=paste0("True Positive ")))

