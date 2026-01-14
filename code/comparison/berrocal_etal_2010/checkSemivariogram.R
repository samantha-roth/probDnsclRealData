rm(list=ls())
graphics.off()
library(nimble)
library(MCMCvis)
library(geostats)  # For semivariogram computation

setwd("/Users/f007f8t/Documents/GitHub/probDnsclRealData")

#load the high water marks
load("data/HWMsdf.RData")
HWMlocs<- as.matrix(HWMs.df[,c("x","y")]); HWMlocs<- HWMlocs[1:5,]
obs<- HWMs.df$height[1:5]

#load the 10m res water surface heights
load("data/wsh.10m_HWMlocs") 

resid<- obs-wsh.10m
semivariogram(x=HWMlocs[,"x"],y=HWMlocs[,"y"],z=resid)

#flat semivariogram --> no evidence for spatial correlation.

semivariogram(x=HWMlocs[,"x"],y=HWMlocs[,"y"],z=obs) 

# increasing semivariogram, but no leveling off point is reached. don't have enough data to find one.


#now check the residuals for the SLR predictions

load("data/mcmc.output1_thinned")

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


SLR_resids<- obs-mean_downscale_vals

semivariogram(x=HWMlocs[,"x"],y=HWMlocs[,"y"],z=SLR_resids)

#maybe some evidence for spatial structure? I am skeptical
