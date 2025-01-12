
rm(list=ls())

library(terra)

ndArgs(trailingOnly=TRUE)
setwd(dir)

load("data/coords.5m.RData")
dem5m<- rast("data/norristown_5m.asc")
elev5m<- values(dem5m)

flood<- c("flood2014","flood2020","floodfuture")

for(f in 1:length(flood)){
  print(flood[f])
  
  #load water surface height downscaled by costgrow
  CG_WSH<- rast(paste0("data/",flood[f],"/CG_WSH.tif"))
  
  #load region of interest coordinates
  load("data/boxAroundHWMs.5m.RData")
  
  #load calibrated 5m flood projections in region of interest
  load(paste0("data/",flood[f],"/vals5minBdsAroundHWMs.RData"))
  
  wshCostGrow<- c(as.matrix(extract(CG_WSH,coordsinBds.5m)))
  wshCostGrow[which(is.na(wshCostGrow))]<-0
  
  MAE.costgrow<- mean(abs(vals5minBds-wshCostGrow)) #.144, slightly higher than ours
  print(paste0("MAE: ",MAE.costgrow))
  #floodedCostGrow<- ifelse(wshCostGrow>0,1,0)
  #flooded5m<- ifelse(vals5minBds>0,1,0)
  
  #sens.costgrow<- length(which(floodedCostGrow==1 & flooded5m==1))/length(which(flooded5m==1))
  #spec.costgrow<- length(which(floodedCostGrow==0 & flooded5m==0))/length(which(flooded5m==0))
  
  ################################################################################
  #now set the minimum threshold for flooding to be 0.3
  
  floodedCostGrow<- ifelse(wshCostGrow>0.3,1,0)
  flooded5m<- ifelse(vals5minBds>0.3,1,0)
  
  sens.costgrow<- length(which(floodedCostGrow==1 & flooded5m==1))/length(which(flooded5m==1))
  spec.costgrow<- length(which(floodedCostGrow==0 & flooded5m==0))/length(which(flooded5m==0))
  print(paste0("sensitivity: ",sens.costgrow))
  print(paste0("specificity: ",spec.costgrow))
  
  accuracy.costgrow<- (length(which(floodedCostGrow==1 & flooded5m==1)) + length(which(floodedCostGrow==0 & flooded5m==0)))/length(flooded5m)
  print(paste0("accuracy: ",accuracy.costgrow))
  
  save(MAE.costgrow,accuracy.costgrow,sens.costgrow,spec.costgrow,file=paste0("data/",flood[f],"/CostGrowMAEaccuracySensSpec.RData"))
}

