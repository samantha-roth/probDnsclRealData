#get the probability of flooding at the locations of interest based on the model
#for probability of flooding

rm(list=ls())

library(terra)

setwd("C:/Users/svr5482")

flow<- c("Q2559.8429","Q2503.2092","Q3681.19006")

for(f in 1:length(flow)){
  
  if(dir.exists(paste0("probabilisticDownscaling/plots/",flow[f]))==F){
    dir.create(paste0("probabilisticDownscaling/plots/",flow[f]))}
  
  ################################################################################
  ################################################################################
  #10m to 5m
  
  pt<-proc.time()
  
  run5m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs5m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  coords.5m<- xyFromCell(run5m,1:ncell(run5m))
  inds.5m<- 1:ncell(run5m)
  
  #load files giving cost of getting from river to point considering elevation
  costbyElev_River5m<- rast("Documents/ArcGIS/Projects/Norristown_HurricaneIda/CostDis_riv5_1.tif")
  
  #load cells of interest outside the low res flooded cells
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/destInds10mat5m.RData"))
  coords.5mDest<- coords.5m[destInds10m,]
  
  #get 5m costs at the destination coords
  costsAtDestCoords5m<- c(as.matrix(extract(costbyElev_River5m,coords.5mDest)))
  
  ################################################################################
  #Now use the cost to flooding probability models to 
  #determine the probability of being flooded 

  #load parameters from the Gaussian processes
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/modelProbFloodbyCost/parsGP.10mto5m.FN1e-5.RData"))
  
  #load training data (costs and flooding probabilities)
  load(paste0("probabilisticDownscaling/data/",flow[f],"/pctFloodedbyCostAndLB_10mto5m.RData"))
  nCost=length(LB)
  ################################################################################
  #center the % flooded
  cost.c<- (LB+250)/max(LB)
  
  #y.c10<- pctFloodedbyCost10m- mean(pctFloodedbyCost10m)
  y.c10<- pctFloodedbyCost10m- mean(pctFloodedbyCost10m)
  
  #make predictions at 5m costs of destination cells
  costsToPred<- costsAtDestCoords5m
  nNewCost= length(costsToPred)
  costsToPred.c<- costsToPred/max(LB)
  
  ################################################################################
  #compute distance matrix with new costs to predict at
  SS=c(costsToPred.c,cost.c)
  AA = matrix(SS,length(SS),length(SS))
  BB = t(AA)
  C1 = abs(AA-BB)
  ################################################################################
  nTrain= nCost
  nTest= length(costsToPred)
  nAll<- nTrain+ nTest
  testInds<- 1:nTest
  
  ################################################################################
  #make predictions at the new cost locations based on the function fit 
  #using 10m flooding probabilities
  zeta=1e-5
  
  cov.red=zeta*diag(1,nAll)+kappa.10*exp(-C1/phi.10^2) #compute the covariance function
  Sigma22=cov.red[-testInds,-testInds] #Sigma_22 matrix
  Sigma12=matrix(cov.red[testInds,-testInds],nTest,nTrain) #Sigma matrix
  predicted=Sigma12%*%solve(Sigma22)%*%y.c10 #predicted values from the emulator
  
  predProbFlood<- predicted + mean(pctFloodedbyCost10m)
  #predProbFlood[which(costsToPred>1e4)]<-0
  
  save(predProbFlood,
       file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/modelProbFloodbyCost/predProbFloodatDest_5mCost10mModel.RData"))
  
  ptFinal<-proc.time()-pt
  time_predProbFlood<-ptFinal[3]
  save(time_predProbFlood, file= paste0("C:/Users/svr5482/probabilisticDownscaling/data/",flow[f],"/time_predProbFlood10mto5m.RData"))
  
  
  filename<- paste0("probabilisticDownscaling/plots/",flow[f],"/ProbFloodbyCostFuncAtDestPts10mto5m.jpeg")
  jpeg(file = filename,width = 700,height=700)
  print(plot(costsToPred,predProbFlood,
       main="Probability Flooded = f(Cost of Getting There from River)",
       xlab="Cost of Getting From River to Destination",
       ylab="Interpolated P(Flooded) From 10m Flooded Locs"))
  print(points(cost.c*max(LB),pctFloodedbyCost10m,col="red",pch=16))
  dev.off()
  
  ################################################################################
  ################################################################################
  #30m to 5m
  
  pt<-proc.time()
  
  run5m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs5m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  coords.5m<- xyFromCell(run5m,1:ncell(run5m))
  inds.5m<- 1:ncell(run5m)
  
  #load files giving cost of getting from river to point considering elevation
  costbyElev_River5m<- rast("probabilisticDownscaling/data/costbyElev_River5m.tif")
  
  #load cells of interest outside the low res flooded cells
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/destInds30mat5m.RData"))
  coords.5mDest<- coords.5m[destInds30m,]
  
  #get 5m costs at the destination coords
  costsAtDestCoords5m<- c(as.matrix(extract(costbyElev_River5m,coords.5mDest)))
  
  ################################################################################
  #Now use the cost to flooding probability models to 
  #determine the probability of being flooded 
  
  #load parameters from the Gaussian processes
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/modelProbFloodbyCost/parsGP.30mto5m.FN1e-5.RData"))
  
  #load training data (costs and flooding probabilities)
  load(paste0("probabilisticDownscaling/data/",flow[f],"/pctFloodedbyCostAndLB_30mto5m.RData"))
  nCost=length(LB)
  ################################################################################
  #center the % flooded
  cost.c<- (LB+250)/max(LB)
  
  #y.c30<- pctFloodedbyCost30m- mean(pctFloodedbyCost30m)
  y.c30<- pctFloodedbyCost30m- mean(pctFloodedbyCost30m)
  
  #make predictions at 5m costs of destination cells
  costsToPred<- costsAtDestCoords5m
  nNewCost= length(costsToPred)
  costsToPred.c<- costsToPred/max(LB)
  
  ################################################################################
  #compute distance matrix with new costs to predict at
  SS=c(costsToPred.c,cost.c)
  AA = matrix(SS,length(SS),length(SS))
  BB = t(AA)
  C1 = abs(AA-BB)
  ################################################################################
  nTrain= nCost
  nTest= length(costsToPred)
  nAll<- nTrain+ nTest
  testInds<- 1:nTest
  
  ################################################################################
  #make predictions at the new cost locations based on the function fit 
  #using 30m flooding probabilities
  zeta=1e-5
  
  cov.red=zeta*diag(1,nAll)+kappa.30*exp(-C1/phi.30^2) #compute the covariance function
  Sigma22=cov.red[-testInds,-testInds] #Sigma_22 matrix
  Sigma12=matrix(cov.red[testInds,-testInds],nTest,nTrain) #Sigma matrix
  predicted=Sigma12%*%solve(Sigma22)%*%y.c30 #predicted values from the emulator
  
  predProbFlood<- predicted + mean(pctFloodedbyCost30m)
  #predProbFlood[which(costsToPred>1e4)]<-0
  
  save(predProbFlood,
       file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/modelProbFloodbyCost/predProbFloodatDest_5mCost30mModel.RData"))
  
  ptFinal<-proc.time()-pt
  time_predProbFlood<-ptFinal[3]
  save(time_predProbFlood, file= paste0("C:/Users/svr5482/probabilisticDownscaling/data/",flow[f],"/time_predProbFlood30mto5m.RData"))
  
  
  filename<- paste0("probabilisticDownscaling/plots/",flow[f],"/ProbFloodbyCostFuncAtDestPts30mto5m.jpeg")
  jpeg(file = filename,width = 700,height=700)
  print(plot(costsToPred,predProbFlood,
       main="Probability Flooded = f(Cost of Getting There from River)",
       xlab="Cost of Getting From River to Destination",
       ylab="Interpolated P(Flooded) From 30m Flooded Locs"))
  print(points(cost.c*max(LB),pctFloodedbyCost30m,col="red",pch=16))
  dev.off()  
  
  ################################################################################
  ################################################################################
  #50m to 5m
  
  pt<-proc.time()
  
  run5m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs5m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  coords.5m<- xyFromCell(run5m,1:ncell(run5m))
  inds.5m<- 1:ncell(run5m)
  
  #load files giving cost of getting from river to point considering elevation
  costbyElev_River5m<- rast("probabilisticDownscaling/data/costbyElev_River5m.tif")
  
  #load cells of interest outside the low res flooded cells
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/destInds50mat5m.RData"))
  coords.5mDest<- coords.5m[destInds50m,]
  
  #get 5m costs at the destination coords
  costsAtDestCoords5m<- c(as.matrix(extract(costbyElev_River5m,coords.5mDest)))
  
  ################################################################################
  #Now use the cost to flooding probability models to 
  #determine the probability of being flooded 
  
  #load parameters from the Gaussian processes
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/modelProbFloodbyCost/parsGP.50mto5m.FN1e-5.RData"))
  
  #load training data (costs and flooding probabilities)
  load(paste0("probabilisticDownscaling/data/",flow[f],"/pctFloodedbyCostAndLB_50mto5m.RData"))
  nCost=length(LB)
  ################################################################################
  #center the % flooded
  cost.c<- (LB+250)/max(LB)
  
  #y.c50<- pctFloodedbyCost50m- mean(pctFloodedbyCost50m)
  y.c50<- pctFloodedbyCost50m- mean(pctFloodedbyCost50m)
  
  #make predictions at 5m costs of destination cells
  costsToPred<- costsAtDestCoords5m
  nNewCost= length(costsToPred)
  costsToPred.c<- costsToPred/max(LB)
  
  ################################################################################
  #compute distance matrix with new costs to predict at
  SS=c(costsToPred.c,cost.c)
  AA = matrix(SS,length(SS),length(SS))
  BB = t(AA)
  C1 = abs(AA-BB)
  ################################################################################
  nTrain= nCost
  nTest= length(costsToPred)
  nAll<- nTrain+ nTest
  testInds<- 1:nTest
  
  ################################################################################
  #make predictions at the new cost locations based on the function fit 
  #using 50m flooding probabilities
  zeta=1e-5
  
  cov.red=zeta*diag(1,nAll)+kappa.50*exp(-C1/phi.50^2) #compute the covariance function
  Sigma22=cov.red[-testInds,-testInds] #Sigma_22 matrix
  Sigma12=matrix(cov.red[testInds,-testInds],nTest,nTrain) #Sigma matrix
  predicted=Sigma12%*%solve(Sigma22)%*%y.c50 #predicted values from the emulator
  
  predProbFlood<- predicted + mean(pctFloodedbyCost50m)
  #predProbFlood[which(costsToPred>1e4)]<-0
  
  save(predProbFlood,
       file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/modelProbFloodbyCost/predProbFloodatDest_5mCost50mModel.RData"))
  
  ptFinal<-proc.time()-pt
  time_predProbFlood<-ptFinal[3]
  save(time_predProbFlood, file= paste0("C:/Users/svr5482/probabilisticDownscaling/data/",flow[f],"/time_predProbFlood50mto5m.RData"))
  
  
  filename<- paste0("probabilisticDownscaling/plots/",flow[f],"/ProbFloodbyCostFuncAtDestPts50mto5m.jpeg")
  jpeg(file = filename,width = 700,height=700)
  print(plot(costsToPred,predProbFlood,
       main="Probability Flooded = f(Cost of Getting There from River)",
       xlab="Cost of Getting From River to Destination",
       ylab="Interpolated P(Flooded) From 50m Flooded Locs"))
  print(points(cost.c*max(LB),pctFloodedbyCost50m,col="red",pch=16))
  dev.off()  
  
}

