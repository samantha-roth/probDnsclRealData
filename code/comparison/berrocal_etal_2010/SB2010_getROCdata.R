#plot the ROC curve for Bayesian SLR

#evaluate the performance of the Bayesian version of SLR for downscaling

rm(list=ls())

# dir<- commandArgs(trailingOnly=TRUE)
# setwd(dir)
# setwd("/Volumes/RothS/probDnsclRealData")
# setwd("/dartfs/rc/lab/R/RothS/probDnsclRealData")
setwd("/Users/f007f8t/Documents/GitHub/probDnsclRealData")

library(terra)

#load region of interest coordinates
load("data/boxAroundHWMs.5m.RData")

#load flooded locations at the two lower resolutions being downscaled
load("data/floodInds10mat5mAroundHWMs.RData")

#load calibrated flood projections at 5m in region of interest
load("data/vals5minBdsAroundHWMs.RData")

#load the calibrated projections at each resolution
run10m<- rast("data/Outputs10m/Run_1.asc")
run5m<- rast("data/Outputs5m/Run_1.asc")

#load the posterior means from SLR fit via MCMC
load("data/mcmc.means1")

#define the vector holding thresholds to be used in the ROC curve
threshold<- seq(0.001,1,by=0.001)


################################################################################
#Next we use SLR to downscale WSH at 10m onto 5m grid

#get flood heights at the right location for each resolution
wsh.5m<- c(as.matrix(extract(run5m,coordsinBds.5m)))
wsh.10m<- c(as.matrix(extract(run10m,coordsinBds.5m)))

whichFlood<- ifelse(wsh.5m>0.3,1,0)

mu<- wsh.10m*mcmc.means1["beta1"] + mcmc.means1["beta0"]

pFlood<- rep(NA, length(wsh.10m))
for(i in 1:length(wsh.10m)){
  pFlood[i]<- 1-pnorm(0.3, mean= mu[i], sd = sqrt(mcmc.means1["sigma2"]))
}

predFloodInds<- matrix(NA, nrow= length(threshold), ncol= length(wsh.5m))
TPR<- rep(NA, length(threshold))
TNR<- rep(NA, length(threshold))

for(t in 1:length(threshold)){
  predFloodInds[t,]<- ifelse(pFlood>=threshold[t],1,0)
  TPR[t]<- length(which(predFloodInds[t,]==1 & whichFlood==1))/length(which(whichFlood==1))
  TNR[t]<- length(which(predFloodInds[t,]==0 & whichFlood==0))/length(which(whichFlood==0))
  
  if(t%%100==0){
    print(paste0("threshold=",threshold[t]))
    print(paste0("TPR=", TPR[t]))
    print(paste0("TNR=", TNR[t]))
  }
}

FPR<- 1-TNR
FNR<- 1-TPR

ROC_data_SB2010<- data.frame("TPR"= TPR,"FPR"= FPR,"TNR"=TNR,"FNR"=FNR)


save(ROC_data_SB2010,file="data/ROC_data_SB2010")

