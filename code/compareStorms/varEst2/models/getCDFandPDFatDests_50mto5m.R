#plot distribution of flood heights for a few wet and dry destination cells

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
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/destInds50mat5m.RData"))
  coords.5mDest<- coords.5m[destInds50m,]
  
  #get the actual flood height at each destination location
  trueDestFloodHeights<- c(as.matrix(extract(run5m,coords.5mDest)))
  
  #Now bring in the probability of being in the mapped-to distribution
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/modelProbFloodbyCost/predProbFloodatDest_5mCost50mModel.RData"))
  #load("probabilisticDownscaling/data/simObs/modelProbFloodbyCost/predProbFloodatDest_50mCost50mModel.RData")
  predProbFlood5mCost<- c(predProbFlood)
  
  ################################################################################
  #plot distribution of flood heights for a few nonflooded destination cells
  
  ################################################################################
  
  #If using the downscaled WSE shifted by the elevation at the destination
  
  #load the mean from the nearest downscaled flooded cell
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/shiftbyelevdnsclFromSourceToDest50mto5m.RData"))
  
  ################################################################################
  #If using the unshifted downscaled version
  
  #load the estimated mean from the nearest downscaled flooded cell
  #load("probabilisticDownscaling/data/simObs/dnsclFromSourceToDest50m.RData")
  
  ################################################################################
  probleq0GivenNotPtMass<- rep(NA,length(meanFromSourceToDest))
  totProbleq0<- rep(NA,length(meanFromSourceToDest))
  
  #load the estimated variance from comparing the downscaled projs to the HWMs
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/interpVarDnsclErrbyMeanAbsElevDiff/predVD50mto5m.RData"))
  
  for(i in 1:length(meanFromSourceToDest)){
    probleq0GivenNotPtMass[i]<- pnorm(0, mean = meanFromSourceToDest[i], sd = sqrt(predVD50mto5m))
    totProbleq0[i]<- (1-predProbFlood5mCost[i])+predProbFlood5mCost[i]*probleq0GivenNotPtMass[i]
  }
  
  
  #get the CDF function for num >=0
  CDFatDestPt<- function(sigma, i, num){
    probleqNumGivenNotPtMass<- pnorm(num, mean = meanFromSourceToDest[i], sd = sqrt(predVD50mto5m))
    totProbleqNum<- (1-predProbFlood5mCost[i])+predProbFlood5mCost[i]*probleqNumGivenNotPtMass
    totProbleqNum
  }
  
  #what's the maximum number that should be plugged into the CDF?
  upper<- max(meanFromSourceToDest)+ 3*sqrt(predVD50mto5m)
  
  
  valsToTest<- seq(0,round(upper+.01,2),by=.01)
  
  CDFmat<- matrix(NA, nrow=length(meanFromSourceToDest), ncol= length(valsToTest))
  
  for(j in 1:length(valsToTest)){
    for(i in 1:length(meanFromSourceToDest)){
      CDFmat[i,j]<- CDFatDestPt(sigma=sqrt(varResHWM50m),i=i,num=valsToTest[j])
    }
  }
  
  save(CDFmat,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/interpVarDnsclErrbyMeanAbsElevDiff/shiftbyelevdnsclCDFmat_50mto5m.RData"))
  
  approxPDFmat<- matrix(NA, nrow=length(meanFromSourceToDest), ncol= length(valsToTest))
  
  for(j in 1:length(valsToTest)){
    for(i in 1:length(meanFromSourceToDest)){
      if(j==1) approxPDFmat[i,j]<-CDFmat[i,j]
      if(j>1) approxPDFmat[i,j]<-CDFmat[i,j]-CDFmat[i,j-1]
    }
  }
  
  
  save(approxPDFmat,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/interpVarDnsclErrbyMeanAbsElevDiff/shiftbyelevdnsclApproxPDFmat_50mto5m.RData"))
  #pick some locations (rows) of interest to look at based on performance
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/interpVarDnsclErrbyMeanAbsElevDiff/estProbFloodIndsofInterest_50mto5m.RData"))
  
  
  ################################################################################
  #get the expectation for each destination cell
  
  meanAtDests<- c(approxPDFmat%*%valsToTest)
  #EX2<-  c((approxPDFmat^2)%*%valsToTest)
  #varAtDests<- EX2- meanAtDests^2
  
  mean(meanAtDests[indsProbNotFloodgeq.5]) #0.1098503
  mean(meanAtDests[indsProbFloodgeq.5]) #0.6351877
  
  mean(meanAtDests[indsFalseNeg]) #0.1479432
  mean(meanAtDests[indsNotFloodCorrect]) #0.1057225
  
  mean(meanAtDests[indsFloodCorrect]) #0.6351877
  
  mean(meanAtDests[c(indsNotFloodCorrect)]) #0.1057225
  mean(meanAtDests[c(indsFalseNeg,indsFloodCorrect)]) #0.3859434
  #okay so maybe my distribution doesn't have a valid variance
  
  save(meanAtDests,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/interpVarDnsclErrbyMeanAbsElevDiff/shiftbyelevdnsclMeanAtDests_50mto5m.RData"))
  
  ptFinal<-proc.time()-pt
  time_getCDFandPDFatDests<-ptFinal[3]
  save(time_getCDFandPDFatDests, file=paste0("C:/Users/svr5482/probabilisticDownscaling/data/",flow[f],"/simObs/interpVarDnsclErrbyMeanAbsElevDiff/time_getCDFandPDFatDests_50mto5m.RData"))
  
  ################################################################################
  ##get the expectation for each destination cell
  
  #meanAtDests<- c(approxPDFmat%*%valsToTest)
  ##EX2<-  c((approxPDFmat^2)%*%valsToTest)
  ##varAtDests<- EX2- meanAtDests^2
  
  #mean(meanAtDests[indsProbNotFloodgeq.5]) #0.1209661
  #mean(meanAtDests[indsProbFloodgeq.5]) #0.7764959
  
  #mean(meanAtDests[indsFalseNeg]) #0.1658482
  #mean(meanAtDests[indsNotFloodCorrect]) #0.115429
  
  #mean(meanAtDests[indsFalsePos]) #0.7720464
  #mean(meanAtDests[indsFloodCorrect]) #0.7876483
  
  #mean(meanAtDests[c(indsFalsePos,indsNotFloodCorrect)])
  #mean(meanAtDests[c(indsFalseNeg,indsFloodCorrect)])
  ##okay so maybe my distribution doesn't have a valid variance
}

