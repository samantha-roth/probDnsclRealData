#evaluate the performance of the Bayesian version of SLR for downscaling

rm(list=ls())

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

library(terra)

pt<- proc.time()

#load region of interest coordinates
load("data/boxAroundHWMs.5m.RData")

#load flooded locations at the two lower resolutions being downscaled
load("data/floodInds10mat5mAroundHWMs.RData")

#load calibrated flood projections at 5m in region of interest
load("data/vals5minBdsAroundHWMs.RData")

#load the calibrated projections at each resolution
run10m<- rast("data/Outputs10m/Run_1.asc")
run5m<- rast("data/Outputs5m/Run_1.asc")

#load the thinned posterior samples from SLR fit via MCMC
load("data/mcmc.output1_thinned")

################################################################################
#Next we use SLR to downscale WSH at 10m onto 5m grid

#get flood heights at the right location for each resolution
wsh.5m<- c(as.matrix(extract(run5m,coordsinBds.5m)))
wsh.10m<- c(as.matrix(extract(run10m,coordsinBds.5m)))

SLR_mcmc_pred_func<- function(step){
  return(wsh.10m*beta1_thin[step] + beta0_thin[step])
}

#get downscaled values using the Bayesian SLR model
downscale_vals<- matrix(NA, nrow=length(thin_inds), ncol= length(wsh.10m))
for(i in 1:length(thin_inds)){
  downscale_vals[i,]<- SLR_mcmc_pred_func(i)
}

downscale_vals[which(downscale_vals<0)]<- 0

load("data/mean_SLR_downscale_vals")
load("data/qs_SLR_downscale_vals")

################################################################################
#MAE and others

mean((wsh.5m-mean_downscale_vals)^2) #MSE
sqrt(mean((wsh.5m-mean_downscale_vals)^2)) #RMSE

mean(abs(wsh.5m-mean_downscale_vals)) #MAE 0.82

################################################################################
#95% PI coverage
covered_SLR<- rep(NA,length(wsh.5m))

for(i in 1:length(wsh.5m)){
  covered_SLR[i]<- wsh.5m[i]>=qs_downscale_vals[1,i] & wsh.5m[i]<=qs_downscale_vals[3,i]
}

mean(covered_SLR) #99%

################################################################################
#sensitivity and specificity

#set the minimum threshold for flooding to be 0.3

#what proportion of mcmc steps were flooded for each 5m resolution grid cell?
flooded_func<- function(x) sum(ifelse(x>0.3,1,0))/length(x)
prop_flooded_SLR<- apply(downscale_vals,2,flooded_func) 
flooded_SLR<- ifelse(prop_flooded_SLR>0.5,1,0) #is the proportion of steps that were flooded >0.5?

flooded5m<- ifelse(wsh.5m>0.3,1,0)

sens.SLR<- length(which(flooded_SLR==1 & flooded5m==1))/length(which(flooded5m==1))
spec.SLR<- length(which(flooded_SLR==0 & flooded5m==0))/length(which(flooded5m==0))
print(paste0("SLR total sensitivity for .3m flood: ", sens.SLR)) #1
print(paste0("SLR total specificity for .3m flood: ", spec.SLR)) #0

accuracy.SLR<- (length(which(flooded_SLR==1 & flooded5m==1)) + length(which(flooded_SLR==0 & flooded5m==0)))/length(flooded5m)
print(paste0("SLR total accuracy for .3m flood: ", accuracy.SLR)) #0.56
