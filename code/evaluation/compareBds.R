#compute bounds of 95% prediction interval 
#for high resultion flood heights within low resolution wet cells

rm(list=ls())

pt<-proc.time()

#load the bilinearly interpolated 10m projections at the HWM locations
dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

load("data/downscale10mto5mAtHWMs.RData")
downscale10mAtHWMs<- downscale10m; rm(downscale10m)

#load the high water marks
load("data/HWMsdf.RData")
HWMlocs<- as.matrix(HWMs.df[,c("x","y")]); HWMlocs<- HWMlocs[1:5,]
obs<- HWMs.df$height[1:5]

obsMinusDnsclPred10m<- obs- downscale10mAtHWMs
varResHWM10m<- var(obsMinusDnsclPred10m)

save(varResHWM10m,file="data/varResHWM10mto5m.RData")

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
#10m- downscaled value unshifted

bdsBox10m<- cbind(downscale10m-1.96*sqrt(varResHWM10m),
                  downscale10m+1.96*sqrt(varResHWM10m))

floodvals5mby10m<- vals5minBds[floodInds10mat5m]
isBtwn5mby10m<- rep(NA,length(floodvals5mby10m))

for(i in 1:length(floodvals5mby10m)){
  isBtwn5mby10m[i]<- floodvals5mby10m[i]>=bdsBox10m[i,1] & floodvals5mby10m[i]<=bdsBox10m[i,2]
}

mean(isBtwn5mby10m) #0.9658885

#cor
cor(downscale10m,floodvals5mby10m) #0.988139
#MAE
mean(abs(downscale10m-floodvals5mby10m)) #0.2193905
#MSE
mean((downscale10m-floodvals5mby10m)^2) #0.2456247

save(bdsBox10m,file="data/bdsdownscale10mto5mAroundHWMs.RData")

################################################################################
#10m- downscaled value unshifted

#get the values of s2 with each HWM held out first
s2CV<- rep(NA, length(obsMinusDnsclPred10m))
for(i in 1:length(s2CV)) s2CV[i]<- var(obsMinusDnsclPred10m[-i])

bdsBox10m<- cbind(downscale10mAtHWMs-1.96*sqrt(s2CV),
                  downscale10mAtHWMs+1.96*sqrt(s2CV))

isBtwn5mby10m<- rep(NA,length(obs))

for(i in 1:length(obs)){
  isBtwn5mby10m[i]<- obs[i]>=bdsBox10m[i,1] & obs[i]<=bdsBox10m[i,2]
}

mean(isBtwn5mby10m) #0.2

#cor
cor(downscale10mAtHWMs,obs) #0.9729816
#MAE
mean(abs(downscale10mAtHWMs-obs)) #0.4094965
#MSE
mean((downscale10mAtHWMs-obs)^2) #0.1937424

save(bdsBox10m,file="data/bdsCVdownscale10mto5mAtHWMs.RData")
save(s2CV,file="data/s2CVdownscale10mto5m.RData")

################################################################################

ptFinal<-proc.time()-pt
time_downscale10mAllBds<-ptFinal[3]
save(time_downscale10mAllBds, file= "data/time_downscale10mto5mAllBds.RData")

#compute width of the prediction interval
2*1.96*sqrt(varResHWM10m) #v1


