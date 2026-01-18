#downscale using SLR for the other floods, assuming that the same relationship between wsh.10m and 
#observed flooding holds for other floods

rm(list=ls())

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

library(terra)

#load region of interest coordinates
load("data/boxAroundHWMs.5m.RData")

#load the thinned posterior samples from SLR fit via MCMC
load("data/mcmc.output1_thinned")

flood<- c("flood2014","flood2020","floodfuture")

for(f in 1:length(flood)){
  
  pt<- proc.time()
  
  #load flooded locations at the two lower resolutions being downscaled
  load(paste0("data/",flood[f],"/floodInds10mat5mAroundHWMs.RData"))
  
  #load calibrated flood projections at 5m in region of interest
  load(paste0("data/",flood[f],"/vals5minBdsAroundHWMs.RData"))
  
  #load the calibrated projections at each resolution
  run10m<- rast(paste0("data/Outputs10m/",flood[f],"/Run_1.asc"))
  
  ################################################################################
  #Next we use bilinear interpolation to get the WSH at 10m on 5m grid
  
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
  
  mean_downscale_vals<- apply(downscale_vals,2,mean)
  qs_downscale_vals<- apply(downscale_vals,2,function(x) quantile(x,probs= c(0.025, 0.5, 0.975)))
  
  save(mean_downscale_vals,file=paste0("data/",flood[f],"/mean_SLR_downscale_vals"))
  save(qs_downscale_vals,file=paste0("data/",flood[f],"/qs_SLR_downscale_vals"))
  
  #downscaling improves performance compared to just adjusting here
  
  ptFinal<-proc.time()-pt
  time_SLRMCMC_downscale10mAroundHWMs<-ptFinal[3]
  save(time_SLRMCMC_downscale10mAroundHWMs, file= paste0("data/",flood[f],"/time_SLRMCMC_downscale10mAroundHWMs.RData"))
  
}
