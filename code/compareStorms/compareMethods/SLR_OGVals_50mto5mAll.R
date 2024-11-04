#look at the performance of the SLR model at the locations within the bounds 

rm(list=ls())

library(terra)

setwd("C:/Users/svr5482")


flow<- c("Q2559.8429","Q2503.2092","Q3681.19006")

for(f in 1:length(flow)){
  
  print(flow[f])
  
  #load predictions at 5m locations in bounds from SLR models
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/compareSLRNoAdj/preds_SLR50mto5m.RData"))
  #load the estimated variance estimated from SLR model for HWMs
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/compareSLRNoAdj/var_SLR50mto5m.RData"))
  
  #load the 5m locations around the business area of interest
  load("probabilisticDownscaling/data/simObs/boxAroundRS20.5m.RData")
  
  #load 5m projections
  run5m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs5m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  
  #get 5m values at locations in business area of interest
  vals5minBds<- c(as.matrix(extract(run5m,coordsinBds.5m)))
  
  bdsBox50m<- cbind(preds_SLR50m-1.96*sqrt(var_SLR50m),
                    preds_SLR50m+1.96*sqrt(var_SLR50m))
  
  isBtwn5mby50m<- rep(NA,length(vals5minBds))
  
  for(i in 1:length(vals5minBds)){
    isBtwn5mby50m[i]<- vals5minBds[i]>=bdsBox50m[i,1] & vals5minBds[i]<=bdsBox50m[i,2]
  }
  
  print("% time in PI")
  print(mean(isBtwn5mby50m)) #0.8976608
  
  #cor
  cor(preds_SLR50m,vals5minBds) #0.9839154
  #MAE
  print("MAE")
  print(mean(abs(preds_SLR50m-vals5minBds))) #0.1270387
  #MSE
  mean((preds_SLR50m-vals5minBds)^2) #0.03822417
  
  
  pNoFlood<- rep(NA,length(vals5minBds))
  
  #get sensitivity and specificity
  for(i in 1:length(vals5minBds)){
    pNoFlood[i]<- pnorm(0, mean= preds_SLR50m[i], sd = sqrt(var_SLR50m))
  }
  
  noFlood5mInds<- which(vals5minBds==0)
  predNoFloodInds<- which(pNoFlood>=.5)
  
  correctNoFloodInds<- intersect(noFlood5mInds,predNoFloodInds)
  
  #specificity
  spec_LRW<- length(correctNoFloodInds)/length(noFlood5mInds)
  print(spec_LRW) #0... BAD
  
  
  flood5mInds<- which(vals5minBds>0)
  predFloodInds<- which(pNoFlood<.5)
  
  correctFloodInds<- intersect(flood5mInds,predFloodInds)
  
  #sensitivity
  sens_LRW<- length(correctFloodInds)/length(flood5mInds)
  print(sens_LRW) #1 just predicts everywhere is flooded
  
}


# "Q2559.8429"
# "% time in PI"
# 0.7680312
# "MAE"
# 0.8213155
# 0
# 1
# "Q2503.2092"
# "% time in PI"
# 0.2356238
# "MAE"
# 0.8691907
# 0
# 1
# "Q3681.19006"
# "% time in PI"
# 0.4439571
# "MAE"
# 1.002311
# 1
# 0.8586239