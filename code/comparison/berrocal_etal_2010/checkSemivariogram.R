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

# resid<- obs-wsh.10m
# 
# res_sv_spher<- semivariogram(x=HWMlocs[,"x"],y=HWMlocs[,"y"],z=resid, model= "spherical", fit=TRUE) 
# 
# res_sv_exp<- semivariogram(x=HWMlocs[,"x"],y=HWMlocs[,"y"],z=resid, model= "exponential", fit=TRUE) 
# 
# res_sv_gaus<- semivariogram(x=HWMlocs[,"x"],y=HWMlocs[,"y"],z=resid, model= "gaussian", fit=TRUE) 
# 
# # no evidence for spatial correlation.
# 
# obs_sv_spher<- semivariogram(x=HWMlocs[,"x"],y=HWMlocs[,"y"],z=obs, model= "spherical", fit=TRUE) 
# 
# obs_sv_exp<- semivariogram(x=HWMlocs[,"x"],y=HWMlocs[,"y"],z=obs, model= "exponential", fit=TRUE) 
# 
# obs_sv_gaus<- semivariogram(x=HWMlocs[,"x"],y=HWMlocs[,"y"],z=obs, model= "gaussian", fit=TRUE) 

# no evidence for spatial correlation.

################################################################################

#now check the residuals for the SLR predictions when SLR is fit via MCMC

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

save(SLR_resids,file="data/BayesianSLR_resids")
################################################################################

# #now check the residuals for the SLR predictions when SLR is fit via MLE
# 
# SLR_fit<- lm(obs~wsh.10m) #to inform priors for beta0 and beta1
# mse<- mean(SLR_fit$residuals^2) #to inform tau2
# 
# SLR_resids<- SLR_fit$residuals
# #results are about the same as when we use SLR fit via MCMC, unsurprisingly
################################################################################

sv_SLR_spher<- geostats::semivariogram(x=HWMlocs[,"x"],y=HWMlocs[,"y"],z=SLR_resids, model= "spherical", fit=TRUE, nb= 4)

sv_SLR_exp<- geostats::semivariogram(x=HWMlocs[,"x"],y=HWMlocs[,"y"],z=SLR_resids, model= "exponential", fit=TRUE, nb= 4)

sv_SLR_gaus<- geostats::semivariogram(x=HWMlocs[,"x"],y=HWMlocs[,"y"],z=SLR_resids, model= "gaussian", fit=TRUE, nb= 4)


sv_exp_fit_pars<- sv_SLR_exp$snr
sv_gaus_fit_pars<- sv_SLR_gaus$snr
sv_spher_fit_pars<- sv_SLR_spher$snr

save(sv_exp_fit_pars,file="data/sv_exp_fit_pars")
save(sv_gaus_fit_pars,file="data/sv_gaus_fit_pars")
save(sv_spher_fit_pars,file="data/sv_spher_fit_pars")
#don't see evidence of spatial correlation from any of the models considered

#partial sill, i.e. sigma^2 in nugget + sigma^2 * exp(-|h|/phi)
est_partial_sill<- as.numeric(sv_exp_fit_pars["sill"])- as.numeric(sv_exp_fit_pars["nugget"])

est_phi<- as.numeric(-sv_exp_fit_pars["range"]/log(0.05))

est_nugget<- as.numeric(sv_exp_fit_pars["nugget"])

save(est_partial_sill, est_phi, est_nugget,file="data/est_expcov_pars")
