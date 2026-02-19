#estimate observational errors based on USGS data

rm(list=ls())
graphics.off()

setwd("/Users/f007f8t/Documents/GitHub/probDnsclRealData")

load("/Users/f007f8t/Documents/GitHub/probDnsclRealData/data/hwmQualities.RData")
#assume that the high water marks follow a normal distribution and the error bounds constitute a 95% confidence interval

conversion= 0.3048 #convert ft to m
hwmQualitiesM<- conversion*c(0.2,0.1,0.1,0.5,0.4,0.1,0.2)

alt_z_s2<- sum(hwmQualitiesM[1:5]^2)/4

save(alt_z_s2,file="data/alt_z_s2.RData")