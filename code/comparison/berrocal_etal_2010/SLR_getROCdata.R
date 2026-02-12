#plot the ROC curve for Bayesian SLR


#evaluate the performance of the Bayesian version of SLR for downscaling

rm(list=ls())

# dir<- commandArgs(trailingOnly=TRUE)
# setwd(dir)
setwd("/Users/f007f8t/Documents/GitHub/probDnsclRealData")

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

#define the vector holding thresholds to be used in the ROC curve
threshold<- seq(0.01,1,by=0.01)

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

################################################################################
#sensitivity and specificity

#set the minimum threshold for flooding to be 0.3

#what proportion of mcmc steps were flooded for each 5m resolution grid cell?
flooded_func<- function(x) sum(ifelse(x>0.3,1,0))/length(x)
prop_flooded_SLR<- apply(downscale_vals,2,flooded_func) 

flooded_SLR<- matrix(NA, nrow=length(threshold),ncol=length(prop_flooded_SLR))
for(t in 1:length(threshold)){
  flooded_SLR[t,]<- ifelse(prop_flooded_SLR>threshold[t],1,0)  #is the proportion of steps that were flooded > threshold[t]?
}

flooded5m<- ifelse(wsh.5m>0.3,1,0)


################################################################################
################################################################################
########################### get TPR and FPR ####################################
################################################################################
################################################################################

#counting flooded as positive and not flooded as negative, need TPR = TP/P and FPR = 1- TN/N

TPRbyThreshold<- rep(NA,length(threshold))
TNRbyThreshold<- rep(NA,length(threshold))

for(t in 1:length(threshold)){
  TPRbyThreshold[t]<- length(which(flooded_SLR[t,]==1 & flooded5m==1))/length(which(flooded5m==1))
  TNRbyThreshold[t]<- length(which(flooded_SLR[t,]==0 & flooded5m==0))/length(which(flooded5m==0))
}


FPRbyThreshold<- 1-TNRbyThreshold

ROC_data_SB2010<- data.frame("TPR"= TPRbyThreshold,"FPR"= FPRbyThreshold)

save(ROC_data_SB2010,file="data/ROC_data_SB2010")