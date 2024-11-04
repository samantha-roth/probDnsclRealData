#get sensitivity and specificity in wet low resolution cell area

rm(list=ls())

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

################################################################################
#load estimated variance
load("data/varResHWM10mto5m.RData")

#load flooded locations at the two lower resolutions being downscaled
load("data/floodInds10mat5mAroundHWMs.RData")

#load region of interest coordinates
load("data/boxAroundHWMs.5m.RData")

#load calibrated 5m flood projections in region of interest
load("data/vals5minBdsAroundHWMs.RData")

#load downscaled calibrated flood projections in region of interest
load("data/downscale10mto5mAroundHWMs.RData")
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
spec_LRW<- length(correctNoFloodInds)/length(noFlood5mInds) #0.8653846

nNoFlood<- length(noFlood5mInds)

#FIT FOR WET CELLS #(A and B) / (A + B - A and B)
length(correctNoFloodInds)/(length(predNoFloodInds) + length(noFlood5mInds) -length(correctNoFloodInds))
#0.1420455
################################################################################
#performance on flooded high resolution cells

flood5mInds<- which(floodvals5mby10m>0)
predFloodInds<- which(pNoFlood<.5)

correctFloodInds<- intersect(flood5mInds,predFloodInds)

#sensitivity
sens_LRW<- length(correctFloodInds)/length(flood5mInds) #0.9982181

nFlood<- length(flood5mInds)

#fit for flooded cells

#FIT FOR WET CELLS #(A and B) / (A + B - A and B)
length(correctFloodInds)/(length(predFloodInds) + length(flood5mInds) -length(correctFloodInds))
#0.9803513

save(spec_LRW,sens_LRW,nFlood,nNoFlood,flood5mInds,noFlood5mInds,
     correctFloodInds,predFloodInds,correctNoFloodInds,predNoFloodInds,
     file="data/wetCellAccuracy10mto5m.RData")

pFloodWet<- 1-pNoFlood
save(pFloodWet,file="data/pFloodWet_10mto5m.RData")

