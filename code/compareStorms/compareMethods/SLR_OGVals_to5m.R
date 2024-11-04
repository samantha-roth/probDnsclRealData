#simplified version of berrocal et al with no spatial dependence
#model for observations based on the model output
#this is just SLR

rm(list=ls())

library(terra)

setwd("C:/Users/svr5482")

if(dir.exists("probabilisticDownscaling/data/simObs/compareSLRNoAdj")==F){
  dir.create("probabilisticDownscaling/data/simObs/compareSLRNoAdj")}

#load the high water marks
load("probabilisticDownscaling/data/simObs/obsWE_RS20.RData")

#load the 5m locations around the business area of interest
load("probabilisticDownscaling/data/simObs/boxAroundRS20.5m.RData")


flow<- c("Q2559.8429","Q2503.2092","Q3681.19006")

for(f in 1:length(flow)){
  
  if(dir.exists(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/compareSLRNoAdj"))==F){
    dir.create(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/compareSLRNoAdj"))}
  
  #load the calibrated projections at each resolution
  run50m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs50m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  run30m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs30m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  run10m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs10m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  run5m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs5m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  
  vals50m<- c(as.matrix(extract(run50m,coords.3mRS)))
  vals30m<- c(as.matrix(extract(run30m,coords.3mRS)))
  vals10m<- c(as.matrix(extract(run10m,coords.3mRS)))
  vals5m<- c(as.matrix(extract(run5m,coords.3mRS)))
  
  vals50minBds<- c(as.matrix(extract(run50m,coordsinBds.5m)))
  vals30minBds<- c(as.matrix(extract(run30m,coordsinBds.5m)))
  vals10minBds<- c(as.matrix(extract(run10m,coordsinBds.5m)))
  vals5minBds<- c(as.matrix(extract(run5m,coordsinBds.5m)))
  
  ################################################################################
  #predict the high water mark values using the elevation-adjusted calibrated projections
  
  #5m
  df5to3.1<- data.frame("obs"=obsWE_RS, "preds"= vals5m)
  fit5to3.1<- lm(obs~ preds, data=df5to3.1)
  summary(fit5to3.1)
  
  #10m
  df10to3.1<- data.frame("obs"=obsWE_RS, "preds"= vals10m)
  fit10to3.1<- lm(obs~ preds, data=df10to3.1)
  summary(fit10to3.1)
  var_SLR10m<- summary(fit10to3.1)$sigma^2
  coef_SLR10m<- coef(fit10to3.1)
  
  data10m<- data.frame("preds"<- vals10minBds)
  preds_SLR10m<- predict(fit10to3.1,data10m)
  
  save(preds_SLR10m,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/compareSLRNoAdj/preds_SLR10mto5m.RData"))
  save(var_SLR10m,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/compareSLRNoAdj/var_SLR10mto5m.RData"))
  save(coef_SLR10m,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/compareSLRNoAdj/coef_SLR10mto5m.RData"))
  
  #30m
  df30to3.1<- data.frame("obs"=obsWE_RS, "preds"= vals30m)
  fit30to3.1<- lm(obs~ preds, data=df30to3.1)
  summary(fit30to3.1)
  var_SLR30m<- summary(fit30to3.1)$sigma^2
  coef_SLR30m<- coef(fit30to3.1)
  
  data30m<- data.frame("preds"<- vals30minBds)
  preds_SLR30m<- predict(fit30to3.1,data30m)
  
  save(preds_SLR30m,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/compareSLRNoAdj/preds_SLR30mto5m.RData"))
  save(var_SLR30m,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/compareSLRNoAdj/var_SLR30mto5m.RData"))
  save(coef_SLR30m,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/compareSLRNoAdj/coef_SLR30mto5m.RData"))
  
  
  #50m
  df50to3.1<- data.frame("obs"=obsWE_RS, "preds"= vals50m)
  fit50to3.1<- lm(obs~ preds, data=df50to3.1)
  summary(fit50to3.1)
  var_SLR50m<- summary(fit50to3.1)$sigma^2
  coef_SLR50m<- coef(fit50to3.1)
  
  data50m<- data.frame("preds"<- vals50minBds)
  preds_SLR50m<- predict(fit50to3.1,data30m)
  
  save(preds_SLR50m,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/compareSLRNoAdj/preds_SLR50mto5m.RData"))
  save(var_SLR50m,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/compareSLRNoAdj/var_SLR50mto5m.RData"))
  save(coef_SLR50m,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/compareSLRNoAdj/coef_SLR50mto5m.RData"))
  
  ################################################################################
  #leave one out cross validation
  
  #predict the high water mark values using the elevation-adjusted calibrated projections
  
  preds_SLR10mCV<- rep(NA, length(obsWE_RS))
  preds_SLR30mCV<- rep(NA, length(obsWE_RS))
  preds_SLR50mCV<- rep(NA, length(obsWE_RS))
  
  var_SLR10mCV<- rep(NA, length(obsWE_RS))
  var_SLR30mCV<- rep(NA, length(obsWE_RS))
  var_SLR50mCV<- rep(NA, length(obsWE_RS))
  
  for(i in 1:length(obsWE_RS)){
    #5m
    #df5to3.1<- data.frame("obs"=obsWE_RS[-i], "preds"= vals5m[-i])
    #fit5to3.1<- lm(obs~ preds, data=df5to3.1)
    #summary(fit5to3.1)
    
    #10m
    df10to3.1<- data.frame("obs"=obsWE_RS, "preds"= vals10m)
    fit10to3.1<- lm(obs~ preds, data=df10to3.1[-i,])
    summary(fit10to3.1)
    var_SLR10mCV[i]<- summary(fit10to3.1)$sigma^2
    #coef_SLR10m[i]<- coef(fit10to3.1)
    preds_SLR10mCV[i]<- predict(fit10to3.1,df10to3.1[i,])
    
    #data10m<- data.frame("preds"<- vals10minBds[-i])
    #preds_SLR10mCV<- predict(fit10to3.1,data10m)
    
    save(preds_SLR10mCV,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/compareSLRNoAdj/preds_SLR10mto5mCV.RData"))
    save(var_SLR10mCV,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/compareSLRNoAdj/var_SLR10mto5mCV.RData"))
    #save(coef_SLR10mCV,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/compareSLRNoAdj/coef_SLR10mto5m.RData"))
    
    #30m
    df30to3.1<- data.frame("obs"=obsWE_RS, "preds"= vals30m)
    fit30to3.1<- lm(obs~ preds, data=df30to3.1[-i,])
    summary(fit30to3.1)
    var_SLR30mCV[i]<- summary(fit30to3.1)$sigma^2
    #coef_SLR30m[i]<- coef(fit30to3.1)
    preds_SLR30mCV[i]<- predict(fit30to3.1,df30to3.1[i,])
    
    #data30m<- data.frame("preds"<- vals30minBds[-i])
    #preds_SLR30mCV<- predict(fit30to3.1,data30m)
    
    save(preds_SLR30mCV,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/compareSLRNoAdj/preds_SLR30mto5mCV.RData"))
    save(var_SLR30mCV,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/compareSLRNoAdj/var_SLR30mto5mCV.RData"))
    #save(coef_SLR30mCV,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/compareSLRNoAdj/coef_SLR30mto5m.RData"))
    
    #50m
    df50to3.1<- data.frame("obs"=obsWE_RS, "preds"= vals50m)
    fit50to3.1<- lm(obs~ preds, data=df50to3.1[-i,])
    summary(fit50to3.1)
    var_SLR50mCV[i]<- summary(fit50to3.1)$sigma^2
    #coef_SLR50m[i]<- coef(fit50to3.1)
    preds_SLR50mCV[i]<- predict(fit50to3.1,df50to3.1[i,])
    
    #data50m<- data.frame("preds"<- vals50minBds[-i])
    #preds_SLR50mCV<- predict(fit50to3.1,data50m)
    
    save(preds_SLR50mCV,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/compareSLRNoAdj/preds_SLR50mto5mCV.RData"))
    save(var_SLR50mCV,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/compareSLRNoAdj/var_SLR50mto5mCV.RData"))
    #save(coef_SLR50mCV,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/compareSLRNoAdj/coef_SLR10mto5m.RData"))
    
  }
  
  
}
