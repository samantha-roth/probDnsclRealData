rm(list=ls())
graphics.off()
library(nimble)
library(MCMCvis)
library(geostats)  # For semivariogram computation

setwd("/Users/f007f8t/Documents/GitHub/probDnsclRealData")

#load the high water marks
load("data/HWMsdf.RData")
HWMlocs<- as.matrix(HWMs.df[,c("x","y")]); HWMlocs<- HWMlocs[1:5,]
obs<- HWMs.df$height[1:5]

#load the 10m res water surface heights
load("data/wsh.10m_HWMlocs") 

resid<- obs-wsh.10m
semivariogram(x=HWMlocs[,"x"],y=HWMlocs[,"y"],z=resid)

#flat semivariogram --> no evidence for spatial correlation.