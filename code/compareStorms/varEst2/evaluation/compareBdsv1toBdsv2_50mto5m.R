
rm(list=ls())

setwd("C:/Users/svr5482")

pt<-proc.time()

#load region of interest coordinates
load("probabilisticDownscaling/data/simObs/boxAroundRS20.5m.RData")

flow<- c("Q2559.8429","Q2503.2092","Q3681.19006")

for(f in 1:length(flow)){
  
  print(flow[f])
  
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/interpVarDnsclErrbyMeanAbsElevDiff/predVD50mto5m.RData"))
  
  #load("probabilisticDownscaling/data/simObs/shiftdnscl50mAtRS20.RData")
  
  #shiftdnscl50mAtRS20<- shiftdnscl50m; rm(shiftdnscl50m)
  
  #obsMinusShiftDnsclPred50m<-obsWE_RS-shiftdnscl50mAtRS20
  #varShiftResHWM50m<- var(obsMinusShiftDnsclPred50m)
  
  #save(varShiftResHWM50m,file="probabilisticDownscaling/data/simObs/varShiftResHWM50m.RData")
  ################################################################################
  
  #load flooded locations at the two lower resolutions being downscaled
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/floodInds50mat5mAroundRS20.RData"))
  
  #load calibrated 5m flood projections in region of interest
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/vals5minBdsAroundRS20.RData"))
  
  #load downscaled calibrated flood projections in region of interest
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/downscale50mto5mAroundRS20.RData"))
  ################################################################################
  #50m- downscaled value unshifted
  
  bdsBox50m<- cbind(downscale50m-1.96*sqrt(predVD50mto5m),
                    downscale50m+1.96*sqrt(predVD50mto5m))
  
  floodvals5mby50m<- vals5minBds[floodInds50mat5m]
  isBtwn5mby50m<- rep(NA,length(floodvals5mby50m))
  
  for(i in 1:length(floodvals5mby50m)){
    isBtwn5mby50m[i]<- floodvals5mby50m[i]>=bdsBox50m[i,1] & floodvals5mby50m[i]<=bdsBox50m[i,2]
  }
  
  print(paste0("Prop. time projection btwn bds:",mean(isBtwn5mby50m))) #0.9758621
  
  #cor
  print(paste0("Corr. btwn downscaled & high res:",cor(downscale50m,floodvals5mby50m))) #0.9474791
  #MAE
  print(paste0("MAE btwn downscaled & high res:",mean(abs(downscale50m-floodvals5mby50m)))) #0.245623
  #MSE
  print(paste0("MSE btwn downscaled & high res:",mean((downscale50m-floodvals5mby50m)^2))) #0.5036816
  
  save(bdsBox50m,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/interpVarDnsclErrbyMeanAbsElevDiff/bdsdownscale50mto5mAroundRS20.RData"))
  
  ################################################################################
  
  ptFinal<-proc.time()-pt
  time_downscale50mAllBds<-ptFinal[3]
  save(time_downscale50mAllBds, file= paste0("probabilisticDownscaling/data/",flow[f],"/simObs/interpVarDnsclErrbyMeanAbsElevDiff/time_downscale50mto5mAllBds.RData"))
  
  
}

#[1] "Q2559.8429"
#[1] "Prop. time projection btwn bds:0.936391437308869"
#[1] "Corr. btwn downscaled & high res:0.871607914632813"
#[1] "MAE btwn downscaled & high res:0.228852934324574"
#[1] "MSE btwn downscaled & high res:0.128029392746477"
#[1] "Q2503.2092"
#[1] "Prop. time projection btwn bds:0.932321315623023"
#[1] "Corr. btwn downscaled & high res:0.888350787201994"
#[1] "MAE btwn downscaled & high res:0.219175428308375"
#[1] "MSE btwn downscaled & high res:0.108581942226658"
#[1] "Q3681.19006"
#[1] "Prop. time projection btwn bds:0.94496644295302"
#[1] "Corr. btwn downscaled & high res:0.853884930839352"
#[1] "MAE btwn downscaled & high res:0.325277573237717"
#[1] "MSE btwn downscaled & high res:0.471461166833398"