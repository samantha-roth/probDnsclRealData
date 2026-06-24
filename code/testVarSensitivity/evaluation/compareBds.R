#compute bounds of 95% prediction interval 
#for high resultion flood heights within low resolution wet cells

rm(list=ls())

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

load("data/downscale10mto5mAtHWMs.RData")
downscale10mAtHWMs<- downscale10m; rm(downscale10m)

#load the high water marks
load("data/HWMsdf.RData")
HWMlocs<- as.matrix(HWMs.df[,c("x","y")]); HWMlocs<- HWMlocs[1:5,]
obs<- HWMs.df$height[1:5]

obsMinusDnsclPred10m<- obs- downscale10mAtHWMs

load("data/var_samples")

################################################################################

#load flooded locations at the two lower resolutions being downscaled
load("data/floodInds10mat5mAroundHWMs.RData")

#load region of interest coordinates
load("data/boxAroundHWMs.5m.RData")

#load calibrated 5m flood projections in region of interest
load("data/vals5minBdsAroundHWMs.RData")

#load downscaled calibrated flood projections in region of interest
load("data/downscale10mto5mAroundHWMs.RData")

################################################################################

for(v in 1: length(var_samples)){
  #10m- downscaled value unshifted
  if(!dir.exists(paste0("data/var_sample",v))){
    dir.create(paste0("data/var_sample",v))}
  
  pt<-proc.time()
  
  varResHWM10m<- var_samples[v]
  
  bdsBox10m<- cbind(downscale10m-1.96*sqrt(varResHWM10m),
                    downscale10m+1.96*sqrt(varResHWM10m))
  
  floodvals5mby10m<- vals5minBds[floodInds10mat5m]
  isBtwn5mby10m<- rep(NA,length(floodvals5mby10m))
  
  for(i in 1:length(floodvals5mby10m)){
    isBtwn5mby10m[i]<- floodvals5mby10m[i]>=bdsBox10m[i,1] & floodvals5mby10m[i]<=bdsBox10m[i,2]
  }
  
  mean(isBtwn5mby10m) #0.9658885
  
  save(bdsBox10m,file=paste0("data/var_sample",v,"/bdsdownscale10mto5mAroundHWMs.RData"))
  
  ptFinal<-proc.time()-pt
  time_downscale10mAllBds<-ptFinal[3]
  save(time_downscale10mAllBds, file= paste0("data/var_sample",v,"/time_downscale10mto5mAllBds.RData"))
  
  #compute width of the prediction interval
  print(paste0("95% CI width: ",2*1.96*sqrt(varResHWM10m))) #v1
}


