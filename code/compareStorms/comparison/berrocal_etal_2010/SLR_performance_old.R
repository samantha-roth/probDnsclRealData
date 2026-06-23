#evaluate how well SLR does for other floods

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
  
  print(flood[f])
  
  #load calibrated flood projections at 5m in region of interest
  load(paste0("data/",flood[f],"/vals5minBdsAroundHWMs.RData"))
  wsh.5m<- vals5minBds; rm(vals5minBds)
  
  #load the calibrated projections at each resolution
  run10m<- rast(paste0("data/Outputs10m/",flood[f],"/Run_1.asc"))
  
  ################################################################################
  
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
  
  load(paste0("data/",flood[f],"/mean_SLR_downscale_vals"))
  load(paste0("data/",flood[f],"/qs_SLR_downscale_vals"))
  
  ################################################################################
  #MAE and others
  
  #mean((wsh.5m-mean_downscale_vals)^2) #MSE
  #sqrt(mean((wsh.5m-mean_downscale_vals)^2)) #RMSE
  
  print(paste0("MAE: ", mean(abs(wsh.5m-mean_downscale_vals)))) 
  
  ################################################################################
  #95% PI coverage
  covered_SLR<- rep(NA,length(wsh.5m))
  
  for(i in 1:length(wsh.5m)){
    covered_SLR[i]<- wsh.5m[i]>=qs_downscale_vals[1,i] & wsh.5m[i]<=qs_downscale_vals[3,i]
  }
  
  print(paste0("95% PI coverage:",mean(covered_SLR)))
  
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
  print(paste0("SLR total sensitivity for .3m flood: ", sens.SLR)) 
  print(paste0("SLR total specificity for .3m flood: ", spec.SLR)) 
  
  accuracy.SLR<- (length(which(flooded_SLR==1 & flooded5m==1)) + length(which(flooded_SLR==0 & flooded5m==0)))/length(flooded5m)
  print(paste0("SLR total accuracy for .3m flood: ", accuracy.SLR)) 
  
  
}