rm(list=ls())

library(terra)

pt<- proc.time()

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

#load region of interest coordinates
load("data/boxAroundHWMs.5m.RData")

#load flooded locations at the two lower resolutions being downscaled
load("data/floodInds10mat5mAroundHWMs.RData")

#load calibrated flood projections at 5m in region of interest
load("data/vals5minBdsAroundHWMs.RData")

#load the calibrated projections at each resolution
run10m<- rast("data/Outputs10m/Run_1.asc")
run5m<- rast("data/Outputs5m/Run_1.asc")


################################################################################
#Next we use SLR to downscale WSH at 10m onto 5m grid

wsh.10m<- c(as.matrix(extract(run10m,coordsinBds.5m)))
wsh.5m<- c(as.matrix(extract(run5m,coordsinBds.5m)))


SLR_mcmc_pred_func<- function(step){
  return(wsh.10m*beta1_thin[step] + beta0_thin[step])
}

#get downscaled values using the Bayesian SLR model
downscale_vals<- matrix(NA, nrow=length(thin_inds), ncol= length(wsh.10m))
for(i in 1:length(thin_inds)){
  downscale_vals[i,]<- SLR_mcmc_pred_func(i)
}

mean_downscale_vals<- apply(downscale_vals,2,mean)
qs_downscale_vals<- apply(downscale_vals,2,function(x) quantile(x,probs= c(0.025, 0.5, 0.975)))


save(mean_downscale_vals,file="data/mean_SLR_downscale_vals")
save(qs_downscale_vals,file="data/qs_SLR_downscale_vals")

ptFinal<-proc.time()-pt
time_SLRMCMC_downscale10mAroundHWMs<-ptFinal[3]
save(time_SLRMCMC_downscale10mAroundHWMs, file= "data/time_SLRMCMC_downscale10mAroundHWMs.RData")

mean((wsh.5m-mean_downscale_vals)^2) #MSE
sqrt(mean((wsh.5m-mean_downscale_vals)^2)) #RMSE

mean(abs(wsh.5m-mean_downscale_vals)) #MAE


