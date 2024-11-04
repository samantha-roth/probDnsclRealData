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
    pNoFlood[i]<- pnorm(0, mean= downscale10m[i], sd = sqrt(varResHWM10m))
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
       file=paste0("data/",flood[f],"/wetCellAccuracy10mto5m.RData"))
  
  pFloodWet<- 1-pNoFlood
  save(pFloodWet,file=paste0("data/",flood[f],"/pFloodWet_10mto5m.RData"))
  
}

# "flood2014"
# "specificity: 0.858490566037736"
# "fit for dry cells: 0.392241379310345"
# "sensitivity: 0.975590856257265"
# "fit for wet cell: 0.972764149121113"
# "flood2020"
# "specificity: 0.896551724137931"
# "fit for dry cells: 0.395939086294416"
# "sensitivity: 0.97783598629861"
# "fit for wet cell: 0.976065969428801"
# "floodfuture"
# "specificity: 0.761904761904762"
# "fit for dry cells: 0.0919540229885057"
# "sensitivity: 0.982111539810593"
# "fit for wet cell: 0.981537742463192"