
rm(list=ls())

setwd("C:/Users/svr5482")

pt<-proc.time()

#load region of interest coordinates
load("probabilisticDownscaling/data/simObs/boxAroundRS20.5m.RData")

flow<- c("Q2559.8429","Q2503.2092","Q3681.19006")

for(f in 1:length(flow)){
  
  print(flow[f])
  
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/interpVarDnsclErrbyMeanAbsElevDiff/predVD10mto5m.RData"))
  
  #load("probabilisticDownscaling/data/simObs/shiftdnscl10mAtRS20.RData")
  
  #shiftdnscl10mAtRS20<- shiftdnscl10m; rm(shiftdnscl10m)
  
  #obsMinusShiftDnsclPred10m<-obsWE_RS-shiftdnscl10mAtRS20
  #varShiftResHWM10m<- var(obsMinusShiftDnsclPred10m)
  
  #save(varShiftResHWM10m,file="probabilisticDownscaling/data/simObs/varShiftResHWM10m.RData")
  ################################################################################
  
  #load flooded locations at the two lower resolutions being downscaled
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/floodInds10mat5mAroundRS20.RData"))
  
  #load calibrated 5m flood projections in region of interest
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/vals5minBdsAroundRS20.RData"))
  
  #load downscaled calibrated flood projections in region of interest
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/downscale10mto5mAroundRS20.RData"))
  ################################################################################
  #10m- downscaled value unshifted
  
  bdsBox10m<- cbind(downscale10m-1.96*sqrt(predVD10mto5m),
                    downscale10m+1.96*sqrt(predVD10mto5m))
  
  floodvals5mby10m<- vals5minBds[floodInds10mat5m]
  isBtwn5mby10m<- rep(NA,length(floodvals5mby10m))
  
  for(i in 1:length(floodvals5mby10m)){
    isBtwn5mby10m[i]<- floodvals5mby10m[i]>=bdsBox10m[i,1] & floodvals5mby10m[i]<=bdsBox10m[i,2]
  }
  
  print(paste0("Prop. time projection btwn bds:",mean(isBtwn5mby10m))) #0.9758621
  
  #cor
  print(paste0("Corr. btwn downscaled & high res:",cor(downscale10m,floodvals5mby10m))) #0.9474791
  #MAE
  print(paste0("MAE btwn downscaled & high res:",mean(abs(downscale10m-floodvals5mby10m)))) #0.245623
  #MSE
  print(paste0("MSE btwn downscaled & high res:",mean((downscale10m-floodvals5mby10m)^2))) #0.1036816
  
  save(bdsBox10m,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/interpVarDnsclErrbyMeanAbsElevDiff/bdsdownscale10mto5mAroundRS20.RData"))

  ################################################################################
  
  ptFinal<-proc.time()-pt
  time_downscale10mAllBds<-ptFinal[3]
  save(time_downscale10mAllBds, file= paste0("probabilisticDownscaling/data/",flow[f],"/simObs/interpVarDnsclErrbyMeanAbsElevDiff/time_downscale10mto5mAllBds.RData"))
  
  #compare the widths of the confidence intervals under each version
  
  #2*1.96*sqrt(varShiftResHWM10m) #v2
  
  #print(2*1.96*sqrt(varResHWM10m)) #v1
  
  #2*1.96*sqrt(varResHWM10m)-2*1.96*sqrt(varShiftResHWM10m)
  
  
}

#[1] "Q2559.8429"
#[1] "Prop. time projection btwn bds:1"
#[1] "Corr. btwn downscaled & high res:0.999075860207485"
#[1] "MAE btwn downscaled & high res:0.0413588541694445"
#[1] "MSE btwn downscaled & high res:0.00209342763214424"
#[1] "Q2503.2092"
#[1] "Prop. time projection btwn bds:0.999484270242393"
#[1] "Corr. btwn downscaled & high res:0.998641292058253"
#[1] "MAE btwn downscaled & high res:0.0283228263975444"
#[1] "MSE btwn downscaled & high res:0.00164130306191961"
#[1] "Q3681.19006"
#[1] "Prop. time projection btwn bds:1"
#[1] "Corr. btwn downscaled & high res:0.999809076852465"
#[1] "MAE btwn downscaled & high res:0.0594308647025284"
#[1] "MSE btwn downscaled & high res:0.00379377360041267"