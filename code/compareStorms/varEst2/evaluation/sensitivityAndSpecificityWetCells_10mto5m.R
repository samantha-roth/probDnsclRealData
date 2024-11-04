#get sensitivity and specificity in wet low resolution cell area

rm(list=ls())

setwd("C:/Users/svr5482")


#load region of interest coordinates
load("probabilisticDownscaling/data/simObs/boxAroundRS20.5m.RData")

flow<- c("Q2559.8429","Q2503.2092","Q3681.19006")

for(f in 1:length(flow)){
  print(flow[f])
  
  #load estimated variance
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/interpVarDnsclErrbyMeanAbsElevDiff/predVD10mto5m.RData"))
  ################################################################################
  #load flooded locations at the two lower resolutions being downscaled
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/floodInds10mat5mAroundRS20.RData"))
  
  #load calibrated 5m flood projections in region of interest
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/vals5minBdsAroundRS20.RData"))
  
  #load downscaled calibrated flood projections in region of interest
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/downscale10mto5mAroundRS20.RData"))
  ################################################################################
  
  floodvals5mby10m<- vals5minBds[floodInds10mat5m]
  
  ################################################################################
  #performance on dry high resolution cells
  
  pNoFlood<- rep(NA, length(downscale10m))
  for(i in 1:length(downscale10m)){
    pNoFlood[i]<- pnorm(0, mean= downscale10m[i], sd = sqrt(predVD10mto5m))
  }
  
  noFlood5mInds<- which(floodvals5mby10m==0)
  predNoFloodInds<- which(pNoFlood>=.5)
  
  correctNoFloodInds<- intersect(noFlood5mInds,predNoFloodInds)
  
  #specificity
  spec_LRW<- length(correctNoFloodInds)/length(noFlood5mInds) #0.6285714
  print(paste0("specificity: ",spec_LRW))
  
  nNoFlood<- length(noFlood5mInds)
  
  #FIT FOR DRY CELLS #(A and B) / (A + B - A and B)
  print(paste0("fit for dry cells: ", length(correctNoFloodInds)/(length(predNoFloodInds) + length(noFlood5mInds) -length(correctNoFloodInds))))
  #0.2365591
  ################################################################################
  #performance on flooded high resolution cells
  
  flood5mInds<- which(floodvals5mby10m>0)
  predFloodInds<- which(pNoFlood<.5)
  
  correctFloodInds<- intersect(flood5mInds,predFloodInds)
  
  #sensitivity
  sens_LRW<- length(correctFloodInds)/length(flood5mInds) #0.9774757
  print(paste0("sensitivity: ",sens_LRW))
  
  nFlood<- length(flood5mInds)
  
  #fit for flooded cells
  
  #FIT FOR WET CELLS #(A and B) / (A + B - A and B)
  print(paste0("fit for wet cell: ",length(correctFloodInds)/(length(predFloodInds) + length(flood5mInds) -length(correctFloodInds))))
  #0.9725657
  
  save(spec_LRW,sens_LRW,nFlood,nNoFlood,flood5mInds,noFlood5mInds,
       correctFloodInds,predFloodInds,correctNoFloodInds,predNoFloodInds,
       file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/interpVarDnsclErrbyMeanAbsElevDiff/wetCellAccuracy10mto5m.RData"))
  
  pFloodWet<- 1-pNoFlood
  save(pFloodWet,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/interpVarDnsclErrbyMeanAbsElevDiff/pFloodWet_10mto5m.RData"))
  
}

#[1] "Q2559.8429"
#[1] "specificity: 0.8"
#[1] "fit for dry cells: 0.736842105263158"
#[1] "sensitivity: 0.996946564885496"
#[1] "fit for wet cell: 0.98989388580091"
#[1] "Q2503.2092"
#[1] "specificity: 0.870967741935484"
#[1] "fit for dry cells: 0.818181818181818"
#[1] "sensitivity: 0.997868939797549"
#[1] "fit for wet cell: 0.993633952254642"
#[1] "Q3681.19006"
#[1] "specificity: 0.872340425531915"
#[1] "fit for dry cells: 0.788461538461538"
#[1] "sensitivity: 0.998422712933754"
#[1] "fit for wet cell: 0.996536523929471"