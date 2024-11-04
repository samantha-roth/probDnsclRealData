rm(list=ls())

library(terra)
library(ggplot2)

setwd("C:/Users/svr5482")

if(dir.exists("probDnsclRealData/plots")==F){
  dir.create("probDnsclRealData/plots")}

#load the SHIFTED bilinearly interpolated 10m projections at the HWM locations
load( "probDnsclRealData/data/downscale10mto5mAtHWMs.RData")

downscale10mAtHWMs<- downscale10m; rm(downscale10m)

#load the high water marks
load("probDnsclRealData/data/HWMsdf.RData")
HWMlocs<- as.matrix(HWMs.df[,c("x","y")]); HWMlocs<- HWMlocs[1:5,]
obs<- HWMs.df$height[1:5]

load("probDnsclRealData/data/bdsCVdownscale10mto5mAtHWMs.RData")
load("probDnsclRealData/data/s2CVdownscale10mto5m.RData")

run5m<- rast("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs5m/Norristown/nCh/PostMedCh/Extent/Run_1.asc")
preds5m<- c(as.matrix(extract(run5m,HWMlocs)))

nLoc=5

df<-data.frame("location"=rep(1:nLoc,5),
               "observation"= rep(obs,5),
               "source"=c(rep("95% PI Bounds",2*nLoc),
                          rep("Downscaled",nLoc),
                          rep("High Resolution",nLoc),
                          rep("Observed",nLoc)),
               "value"=c(bdsBox10m[,1],bdsBox10m[,2],
                         downscale10mAtHWMs,preds5m,
                         obs))

ggplot(df, aes(x = location, y = value, group=source)) + 
  geom_point(aes(shape=source, color=source,size=source)) +
  scale_size_manual(values=c(3,rep(7,3)))+
  scale_color_manual(values = c("gray","red","blue","black")) +
  scale_shape_manual(values=c(15,19,21,16))+
  geom_segment(aes(x = 1, y = bdsBox10m[1,1], xend = 1, yend = bdsBox10m[1,2]), colour="gray", lwd = 1)+
  geom_segment(aes(x = 2, y = bdsBox10m[2,1], xend = 2, yend = bdsBox10m[2,2]), colour="gray", lwd = 1)+
  geom_segment(aes(x = 3, y = bdsBox10m[3,1], xend = 3, yend = bdsBox10m[3,2]), colour="gray", lwd = 1)+
  geom_segment(aes(x = 4, y = bdsBox10m[4,1], xend = 4, yend = bdsBox10m[4,2]), colour="gray", lwd = 1)+
  geom_segment(aes(x = 5, y = bdsBox10m[5,1], xend = 5, yend = bdsBox10m[5,2]), colour="gray", lwd = 1)+
  #geom_line(aes(group = location))+
  #ggtitle("95% Prediction Interval Coverage for Flood Height")+ 
  xlab("Location Number") + ylab("Flood Height (m)") +
  theme_bw() +
  theme(plot.title = element_text(size=24), 
        axis.title = element_text(size=24),
        axis.text = element_text(size = 24),
        legend.text= element_text(size=24),
        legend.title= element_text(size=24))

filename<- "probDnsclRealData/plots/CVdnsclBdsatHWMs_10mto5m.jpeg"
jpeg(file = filename, width = 800,height=500)
ggplot(df, aes(x = location, y = value, group=source)) + 
  geom_point(aes(shape=source, color=source,size=source)) +
  scale_size_manual(values=c(3,rep(7,3)))+
  scale_color_manual(values = c("gray","red","blue","black")) +
  scale_shape_manual(values=c(15,19,21,16))+
  geom_segment(aes(x = 1, y = bdsBox10m[1,1], xend = 1, yend = bdsBox10m[1,2]), colour="gray", lwd = 1)+
  geom_segment(aes(x = 2, y = bdsBox10m[2,1], xend = 2, yend = bdsBox10m[2,2]), colour="gray", lwd = 1)+
  geom_segment(aes(x = 3, y = bdsBox10m[3,1], xend = 3, yend = bdsBox10m[3,2]), colour="gray", lwd = 1)+
  geom_segment(aes(x = 4, y = bdsBox10m[4,1], xend = 4, yend = bdsBox10m[4,2]), colour="gray", lwd = 1)+
  geom_segment(aes(x = 5, y = bdsBox10m[5,1], xend = 5, yend = bdsBox10m[5,2]), colour="gray", lwd = 1)+
  #geom_line(aes(group = location))+
  #ggtitle("95% Prediction Interval Coverage for Flood Height")+ 
  xlab("Location Number") + ylab("Flood Height (m)") +
  theme_bw() +
  theme(plot.title = element_text(size=24), 
        axis.title = element_text(size=24),
        axis.text = element_text(size = 24),
        legend.text= element_text(size=24),
        legend.title= element_text(size=24))
dev.off()
