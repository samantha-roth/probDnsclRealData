rm(list=ls())

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

library(terra)
library(ggplot2)

#load the bilinearly interpolated AND SHIFTED 10m projections at the HWM locations
load("data/downscale10mto5mAtHWMs.RData")

downscale10mAtHWMs<- downscale10m; rm(downscale10m)

#load the high water marks
load("data/HWMsdf.RData")
HWMlocs<- as.matrix(HWMs.df[,c("x","y")]); HWMlocs<- HWMlocs[1:5,]
obs<- HWMs.df$height[1:5]

#get 5m preds at HWM locations
run5m<- rast("data/Outputs5m/Run_1.asc")
preds5matHWMs<- c(as.matrix(extract(run5m,HWMlocs)))

run10m<- rast("data/Outputs10m/Run_1.asc")
preds10matHWMs<- c(as.matrix(extract(run10m,HWMlocs)))


predsVSobs<- data.frame("obs"=obs,
                        "preds10m"=preds10matHWMs,
                        "preds5m"=preds5matHWMs,
                        "downscale10mto5m"=downscale10mAtHWMs)

ggplot(data=predsVSobs,aes(x=preds10m,y=obs))+
  geom_point(color="red")+
  geom_abline(intercept=0,slope=1)

ggplot(data=predsVSobs,aes(x=downscale10mto5m,y=obs))+
  geom_point(color="red")+
  geom_abline(intercept=0,slope=1)

filename<- paste0("plots/pred5mVSobs.jpeg")
jpeg(file = filename,width = 600,height=500)
print(ggplot(data=predsVSobs,aes(x=preds5m,y=obs))+
  geom_point(color="red",size=5)+
  geom_abline(intercept=0,slope=1,lwd = 2) +
  ylab("Observed Flood Height (m)")+ xlab("5m Resolution Predicted Flood Height (m)")+
  theme_bw() + 
  theme(plot.title = element_text(size=24), 
        axis.title = element_text(size=24),
        axis.text = element_text(size = 20),
        legend.text= element_text(size=24),
        legend.title= element_text(size=24)))
dev.off()

mean(abs(preds5matHWMs-obs))
