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

#load the mcmc samples from fitting the SLR model
load("data/mcmc.output1")

beta0_samples<- c(mcmc.output1$chain1[,"beta0"],
                  mcmc.output1$chain2[,"beta0"],
                  mcmc.output1$chain3[,"beta0"],
                  mcmc.output1$chain4[,"beta0"])

beta1_samples<- c(mcmc.output1$chain1[,"beta1"],
                  mcmc.output1$chain2[,"beta1"],
                  mcmc.output1$chain3[,"beta1"],
                  mcmc.output1$chain4[,"beta1"])

sigma2_samples<- c(mcmc.output1$chain1[,"sigma2"],
                  mcmc.output1$chain2[,"sigma2"],
                  mcmc.output1$chain3[,"sigma2"],
                  mcmc.output1$chain4[,"sigma2"])

thin_inds<- 36*(1:1000)

beta0_thin<- beta0_samples[thin_inds]
beta1_thin<- beta1_samples[thin_inds]
sigma2_thin<- sigma2_samples[thin_inds]

#compare thinned samples to originals
quantile(beta0_samples, probs=c(0.025,0.1,0.5,0.9,0.975))
quantile(beta0_thin, probs=c(0.025,0.1,0.5,0.9,0.975))
plot(density(beta0_samples)); lines(density(beta0_thin),col="red")

quantile(beta1_samples, probs=c(0.025,0.1,0.5,0.9,0.975))
quantile(beta1_thin, probs=c(0.025,0.1,0.5,0.9,0.975))
plot(density(beta1_samples)); lines(density(beta1_thin),col="red")

quantile(sigma2_samples, probs=c(0.025,0.1,0.5,0.9,0.975))
quantile(sigma2_thin, probs=c(0.025,0.1,0.5,0.9,0.975))
plot(density(sigma2_samples)); lines(density(sigma2_thin),col="red")

#all look close enough to being the same

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


mean((wsh.5m-mean_downscale_vals)^2) #MSE
sqrt(mean((wsh.5m-mean_downscale_vals)^2)) #RMSE

mean(abs(wsh.5m-mean_downscale_vals)) #MAE
