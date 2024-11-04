
rm(list=ls())

setwd("C:/Users/svr5482")

pt<-proc.time()

#load region of interest coordinates
load("probabilisticDownscaling/data/simObs/boxAroundRS20.5m.RData")

load("probabilisticDownscaling/data/simObs/varResHWM30mto5m.RData")

flow<- c("Q2559.8429","Q2503.2092","Q3681.19006")

for(f in 1:length(flow)){
  
  print(flow[f])
  
  #load("probabilisticDownscaling/data/simObs/shiftdnscl30mAtRS20.RData")
  
  #shiftdnscl30mAtRS20<- shiftdnscl30m; rm(shiftdnscl30m)
  
  #obsMinusShiftDnsclPred30m<-obsWE_RS-shiftdnscl30mAtRS20
  #varShiftResHWM30m<- var(obsMinusShiftDnsclPred30m)
  
  #save(varShiftResHWM30m,file="probabilisticDownscaling/data/simObs/varShiftResHWM30m.RData")
  ################################################################################
  
  #load flooded locations at the two lower resolutions being downscaled
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/floodInds30mat5mAroundRS20.RData"))
  
  #load calibrated 5m flood projections in region of interest
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/vals5minBdsAroundRS20.RData"))
  
  #load downscaled calibrated flood projections in region of interest
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/downscale30mto5mAroundRS20.RData"))
  ################################################################################
  #30m- downscaled value unshifted
  
  bdsBox30m<- cbind(downscale30m-1.96*sqrt(varResHWM30m),
                    downscale30m+1.96*sqrt(varResHWM30m))
  
  floodvals5mby30m<- vals5minBds[floodInds30mat5m]
  isBtwn5mby30m<- rep(NA,length(floodvals5mby30m))
  
  for(i in 1:length(floodvals5mby30m)){
    isBtwn5mby30m[i]<- floodvals5mby30m[i]>=bdsBox30m[i,1] & floodvals5mby30m[i]<=bdsBox30m[i,2]
  }
  
  print(paste0("Prop. time projection btwn bds:",mean(isBtwn5mby30m))) #0.9758621
  
  #cor
  print(paste0("Corr. btwn downscaled & high res:",cor(downscale30m,floodvals5mby30m))) #0.9474791
  #MAE
  print(paste0("MAE btwn downscaled & high res:",mean(abs(downscale30m-floodvals5mby30m)))) #0.245623
  #MSE
  print(paste0("MSE btwn downscaled & high res:",mean((downscale30m-floodvals5mby30m)^2))) #0.1036816
  
  save(bdsBox30m,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/bdsdownscale30mto5mAroundRS20.RData"))
  ################################################################################
  #30m- downscaled value shifted
  #load("C:/Users/svr5482/probabilisticDownscaling/data/simObs/shiftdnscl30mAroundRS20.RData")
  ##load("C:/Users/svr5482/probabilisticDownscaling/data/simObs/coefsToShiftDnscl30mby.RData")
  
  
  #bdsBox30m<- cbind(shiftdnscl30m-1.96*sqrt(varShiftResHWM30m),
  #                  shiftdnscl30m+1.96*sqrt(varShiftResHWM30m))
  
  #floodvals5mby30m<- vals5minBds[floodInds30mat5m]
  #isBtwn5mby30m<- rep(NA,length(floodvals5mby30m))
  
  #for(i in 1:length(floodvals5mby30m)){
  #  isBtwn5mby30m[i]<- floodvals5mby30m[i]>=bdsBox30m[i,1] & floodvals5mby30m[i]<=bdsBox30m[i,2]
  #}
  
  #mean(isBtwn5mby30m) #0.945977
  
  #save(bdsBox30m,file="probabilisticDownscaling/data/simObs/bdsshiftdnscl30mAroundRS20.RData")
  ################################################################################
  
  ptFinal<-proc.time()-pt
  time_downscale30mAllBds<-ptFinal[3]
  save(time_downscale30mAllBds, file= paste0("probabilisticDownscaling/data/",flow[f],"/simObs/time_downscale30mto5mAllBds.RData"))
  
  #compare the widths of the confidence intervals under each version
  
  #2*1.96*sqrt(varShiftResHWM30m) #v2
  
  #print(2*1.96*sqrt(varResHWM30m)) #v1
  
  #2*1.96*sqrt(varResHWM30m)-2*1.96*sqrt(varShiftResHWM30m)
  
  
}

#[1] "Q2559.8429"
#[1] "Prop. time projection btwn bds:0.97576070139247"
#[1] "Corr. btwn downscaled & high res:0.914676539437416"
#[1] "MAE btwn downscaled & high res:0.232620392236498"
#[1] "MSE btwn downscaled & high res:0.0982755624502101"
#[1] "Q2503.2092"
#[1] "Prop. time projection btwn bds:0.973937677053824"
#[1] "Corr. btwn downscaled & high res:0.907977600680024"
#[1] "MAE btwn downscaled & high res:0.228931473664829"
#[1] "MSE btwn downscaled & high res:0.0959763005397721"
#[1] "Q3681.19006"
#[1] "Prop. time projection btwn bds:0.906886517943744"
#[1] "Corr. btwn downscaled & high res:0.940797461600538"
#[1] "MAE btwn downscaled & high res:0.333645237346233"
#[1] "MSE btwn downscaled & high res:0.221242577459005"