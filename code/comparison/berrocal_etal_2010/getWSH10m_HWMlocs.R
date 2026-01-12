#explore how to fit their model with the data we have available

rm(list=ls())
graphics.off()

library(terra)

setwd("/Users/f007f8t/Documents/GitHub/probDnsclRealData")

#load the high water marks
load("data/HWMsdf.RData")
HWMlocs<- as.matrix(HWMs.df[,c("x","y")]); HWMlocs<- HWMlocs[1:5,]
obs<- HWMs.df$height[1:5]

#load the calibrated projections at each resolution
run10m<- rast("data/Outputs10m/Run_1.asc")

load("data/coords.10m.RData")

#extract the water surface height from the 10m resolution model 
#at the location of the high water marks
wsh.10m<- c(as.matrix(extract(run10m,HWMlocs)))

save(wsh.10m, file=paste0("data/wsh.10m_HWMlocs"))
