
rm(list=ls())

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

pt<-proc.time()

#load region of interest coordinates
load("data/boxAroundHWMs.5m.RData")

load("data/varResHWM10mto5m.RData")

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
  
  save(bdsBox10m,file=paste0("data/",flood[f],"/bdsdownscale10mto5mAroundHWMs.RData"))
  ################################################################################

  ptFinal<-proc.time()-pt
  time_downscale10mAllBds<-ptFinal[3]
  save(time_downscale10mAllBds, file= paste0("data/",flood[f],"/time_downscale10mto5mAllBds.RData"))

}

# "flood2014"
# "Prop. time projection btwn bds:0.952733485193622"
# "Corr. btwn downscaled & high res:0.986079305664506"
# "MAE btwn downscaled & high res:0.219612128367305"
# "MSE btwn downscaled & high res:0.223026028853445"
# "flood2020"
# "Prop. time projection btwn bds:0.951287128712871"
# "Corr. btwn downscaled & high res:0.985488261642151"
# "MAE btwn downscaled & high res:0.218941737300598"
# "MSE btwn downscaled & high res:0.219211621527441"
# "floodfuture"
# "Prop. time projection btwn bds:0.964893865173781"
# "Corr. btwn downscaled & high res:0.984535555817578"
# "MAE btwn downscaled & high res:0.240384641532807"
# "MSE btwn downscaled & high res:0.3318466026315"