
#SLR- i.e. stripped down Berrocal et al 2010

rm(list=ls())
graphics.off()

library(terra)

setwd("/Users/f007f8t/Documents/GitHub/probDnsclRealData")

#load the high water marks
load("data/HWMsdf.RData")
HWMlocs<- as.matrix(HWMs.df[,c("x","y")]); HWMlocs<- HWMlocs[1:5,]
obs<- HWMs.df$height[1:5]

#load the 10m res water surface heights
load("data/wsh.10m_HWMlocs")

fit<- lm(obs~ wsh.10m)