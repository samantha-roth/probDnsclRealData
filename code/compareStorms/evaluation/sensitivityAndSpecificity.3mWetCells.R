#get sensitivity and specificity in wet low resolution cell area

rm(list=ls())

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

#load estimated variance
load("data/varResHWM10mto5m.RData")

#load region of interest coordinates
load("data/boxAroundHWMs.5m.RData")

flood<- c("flood2014","flood2020","floodfuture")

for(f in 1:length(flood)){
  print(flood[f])
  
  ################################################################################
  #load flooded locations at the two lower resolutions being downscaled
  load(paste0("data/",flood[f],"/floodInds10mat5mAroundHWMs.RData"))
  
  #load calibrated 5m flood projections in region of interest
  load(paste0("data/",flood[f],"/vals5minBdsAroundHWMs.RData"))
  
  #load downscaled calibrated flood projections in region of interest
  load(paste0("data/",flood[f],"/downscale10mto5mAroundHWMs.RData"))
  ################################################################################
  
  floodvals5mby10m<- vals5minBds[floodInds10mat5m]
  
  ################################################################################
  #performance on dry high resolution cells
  
  pNoFlood<- rep(NA, length(downscale10m))
  for(i in 1:length(downscale10m)){
    pNoFlood[i]<- pnorm(0.3, mean= downscale10m[i], sd = sqrt(varResHWM10m))
  }
  
  noFlood5mInds<- which(floodvals5mby10m<=.3)
  predNoFloodInds<- which(pNoFlood>=.5)
  
  correctNoFloodInds<- intersect(noFlood5mInds,predNoFloodInds)
  
  #specificity
  spec_LRW<- length(correctNoFloodInds)/length(noFlood5mInds) #0.6285714
  print(paste0("specificity: ",spec_LRW))
  
  nNoFlood<- length(noFlood5mInds)
  
  ################################################################################
  #performance on flooded high resolution cells
  
  flood5mInds<- which(floodvals5mby10m>0.3)
  predFloodInds<- which(pNoFlood<.5)
  
  correctFloodInds<- intersect(flood5mInds,predFloodInds)
  
  #sensitivity
  sens_LRW<- length(correctFloodInds)/length(flood5mInds) #0.9774757
  print(paste0("sensitivity: ",sens_LRW))
  
  nFlood<- length(flood5mInds)
  
  #fit for flooded cells
  
  save(spec_LRW,sens_LRW,nFlood,nNoFlood,flood5mInds,noFlood5mInds,
       correctFloodInds,predFloodInds,correctNoFloodInds,predNoFloodInds,
       file=paste0("data/",flood[f],"/wetCellAccuracy_.3mflood_10mto5m.RData"))
  
  pFloodWet<- 1-pNoFlood
  save(pFloodWet,file=paste0("data/",flood[f],"/p.3mFloodWet_10mto5m.RData"))
  
}

# "flood2014"
# "specificity: 0.986206896551724"
# "sensitivity: 0.949018771331058"
# "flood2020"
# "specificity: 0.994515539305302"
# "sensitivity: 0.956473462136354"
# "floodfuture"
# "specificity: 0.922413793103448"
# "sensitivity: 0.973279735161977"