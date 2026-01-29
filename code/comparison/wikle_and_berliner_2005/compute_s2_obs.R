#estimate observational errors based on USGS data

rm(list=ls())
graphics.off()

setwd("/Users/f007f8t/Documents/GitHub/probDnsclRealData")

load("/Users/f007f8t/Documents/GitHub/probDnsclRealData/data/hwmQualities.RData")
#assume that the high water marks follow a normal distribution and the error bounds constitute a 95% confidence interval

conversion= 0.3048 #convert ft to m
hwmQualitiesM<- conversion*c(0.2,0.1,0.1,0.5,0.4,0.1,0.2)

z_s2_est95= (hwmQualitiesM/1.96)^2

save(z_s2_est95,file="data/obserror_est.RData")
