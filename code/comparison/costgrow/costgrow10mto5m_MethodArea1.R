
rm(list=ls())

library(terra)

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

costgrow_WSE<- rast("data/costgrow10mto5m_methodArea1.tif")
#plot(costgrow_WSE)

dem5m<- rast("data/norristown_5m.asc")
elev5m<- values(dem5m)

coords.costgrow<- xyFromCell(costgrow_WSE,1:ncell(costgrow_WSE))

coords.dem5m<- xyFromCell(dem5m,1:ncell(dem5m))

cgvals10mto5m<- values(costgrow_WSE)

wshCostGrow10mto5mVals<- cgvals10mto5m-elev5m

wshCostGrow10mto5mVals[which(is.na(wshCostGrow10mto5mVals))]<-0

wshCostGrow10mto5m<- dem5m
values(wshCostGrow10mto5m)<- wshCostGrow10mto5mVals

plot(wshCostGrow10mto5m)

################################################################################
#crop to area of interest and compare to 5m projections

#load region of interest coordinates
load("data/boxAroundHWMs.5m.RData")

wshCostGrow<- c(as.matrix(extract(wshCostGrow10mto5m,coordsinBds.5m)))

run5m<- rast("data/Outputs5m/Run_1.asc")
wsh5m<- c(as.matrix(extract(run5m,coordsinBds.5m)))

MAE.costgrow<- mean(abs(wsh5m-wshCostGrow)) #.144, slightly higher than ours
print(paste0("CostGrow MAE: ", MAE.costgrow))

floodedCostGrow<- ifelse(wshCostGrow>0,1,0)
flooded5m<- ifelse(wsh5m>0,1,0)

sens.costgrow<- length(which(floodedCostGrow==1 & flooded5m==1))/length(which(flooded5m==1))
spec.costgrow<- length(which(floodedCostGrow==0 & flooded5m==0))/length(which(flooded5m==0))

################################################################################
#now set the minimum threshold for flooding to be 0.3

floodedCostGrow<- ifelse(wshCostGrow>0.3,1,0)
flooded5m<- ifelse(wsh5m>0.3,1,0)

sens.costgrow<- length(which(floodedCostGrow==1 & flooded5m==1))/length(which(flooded5m==1))
spec.costgrow<- length(which(floodedCostGrow==0 & flooded5m==0))/length(which(flooded5m==0))
print(paste0("CostGrow total sensitivity for .3m flood: ", sens.costgrow))
print(paste0("CostGrow total specificity for .3m flood: ", spec.costgrow))

accuracy.costgrow<- (length(which(floodedCostGrow==1 & flooded5m==1)) + length(which(floodedCostGrow==0 & flooded5m==0)))/length(flooded5m)
print(paste0("CostGrow total accuracy for .3m flood: ", accuracy.costgrow))

save(MAE.costgrow,accuracy.costgrow,sens.costgrow,spec.costgrow,file="data/CostGrowMAEaccuracySensSpec.RData")

