
rm(list=ls())

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

pt<-proc.time()

#load region of interest coordinates
load("data/boxAroundHWMs.5m.RData")

#load the sampled variances from the empirical distribution 
load("data/var_samples")

flood<- c("flood2014","flood2020","floodfuture")

for(f in 1:length(flood)){
  
  print(flood[f])
  
  
  #load flooded locations at the two lower resolutions being downscaled
  load(paste0("data/",flood[f],"/floodInds10mat5mAroundHWMs.RData"))
  
  #load calibrated 5m flood projections in region of interest
  load(paste0("data/",flood[f],"/vals5minBdsAroundHWMs.RData"))
  
  #load downscaled calibrated flood projections in region of interest
  load(paste0("data/",flood[f],"/downscale10mto5mAroundHWMs.RData"))
  ################################################################################
  
  for(v in 1: length(var_samples)){
    
    varResHWM10m<- var_samples[v]
    
    print(paste0("var sample= ",var_samples[v]))
    
    if(!dir.exists(paste0("data/",flood[f],"/var_sample",v))){
      dir.create(paste0("data/",flood[f],"/var_sample",v))}
    
    #10m- downscaled value unshifted
    
    bdsBox10m<- cbind(downscale10m-1.96*sqrt(varResHWM10m),
                      downscale10m+1.96*sqrt(varResHWM10m))
    
    floodvals5mby10m<- vals5minBds[floodInds10mat5m]
    isBtwn5mby10m<- rep(NA,length(floodvals5mby10m))
    
    for(i in 1:length(floodvals5mby10m)){
      isBtwn5mby10m[i]<- floodvals5mby10m[i]>=bdsBox10m[i,1] & floodvals5mby10m[i]<=bdsBox10m[i,2]
    }
    
    print(paste0("Prop. time projection btwn bds:",mean(isBtwn5mby10m))) #0.9758621
    
    #cor
    print(paste0("Corr. btwn downscaled & high res:",cor(downscale10m,floodvals5mby10m))) #0.9474791
    #MAE
    print(paste0("MAE btwn downscaled & high res:",mean(abs(downscale10m-floodvals5mby10m)))) #0.245623
    #MSE
    print(paste0("MSE btwn downscaled & high res:",mean((downscale10m-floodvals5mby10m)^2))) #0.1036816
    
    save(bdsBox10m,file=paste0("data/",flood[f],"/var_sample",v,"/bdsdownscale10mto5mAroundHWMs.RData"))
    ################################################################################
    
    ptFinal<-proc.time()-pt
    time_downscale10mAllBds<-ptFinal[3]
    save(time_downscale10mAllBds, file= paste0("data/",flood[f],"/var_sample",v,"/time_downscale10mto5mAllBds.RData"))
    
  }

}

