
rm(list=ls())
graphics.off()

setwd("/Users/f007f8t/Documents/GitHub/probDnsclRealData")

#setwd("/Users/f007f8t/Documents/GitHub/probDnsclRealData")

library(terra)
library(ggplot2)
library(ape)

#load the high water marks
load("data/HWMsdf.RData")
HWMlocs<- as.matrix(HWMs.df[,c("x","y")]); HWMlocs<- HWMlocs[1:5,]
obs<- HWMs.df$height[1:5]

#load the bilinearly interpolated AND SHIFTED 10m projections at the HWM locations
load("data/downscale10mto5mAtHWMs.RData")

downscale10mAtHWMs<- downscale10m; rm(downscale10m)
downscale_resids<- obs- downscale10mAtHWMs

#get 5m preds at HWM locations
run5m<- rast("data/Outputs5m/Run_1.asc")
preds5matHWMs<- c(as.matrix(extract(run5m,HWMlocs)))
preds5m_resids<- obs- preds5matHWMs

run10m<- rast("data/Outputs10m/Run_1.asc")
preds10matHWMs<- c(as.matrix(extract(run10m,HWMlocs)))
preds10m_resids<- obs- preds10matHWMs

#load the residuals from regressing the high water marks on the low resolution flood heights
load("data/BayesianSLR_resids")

preds_obs_locs.df<- data.frame("obs"=obs,
                               "preds10m_resids"=preds10m_resids,
                               "preds5m_resids"=preds5m_resids,
                               "downscale_resids"= downscale_resids,
                               "SLR_resids"= SLR_resids,
                               "x"=HWMlocs[,"x"],
                               "y"=HWMlocs[,"y"])


#compute distance matrix using euclidean distance for the locations
loc.dist_euclidean<- as.matrix(dist(HWMlocs,method="euclidean"))

#compute distance matrix using manhattan distance for the locations
loc.dist_manhattan<- as.matrix(dist(HWMlocs,method="manhattan"))

#get inverse distance weights
loc.dist_euclidean.inv <- 1/loc.dist_euclidean
diag(loc.dist_euclidean.inv) <- 0

loc.dist_manhattan.inv <- 1/loc.dist_manhattan
diag(loc.dist_manhattan.inv) <- 0

#calculate Moran's I for obs and different residuals using euclidean distance IDWs
Moran.I(obs, loc.dist_euclidean.inv)$p.value
Moran.I(SLR_resids, loc.dist_euclidean.inv)$p.value
Moran.I(downscale_resids, loc.dist_euclidean.inv)$p.value
Moran.I(preds10m_resids, loc.dist_euclidean.inv)$p.value
Moran.I(preds5m_resids, loc.dist_euclidean.inv)$p.value

#calculate Moran's I for obs and different residuals using manhattan distance IDWs
Moran.I(obs, loc.dist_manhattan.inv)$p.value
Moran.I(SLR_resids, loc.dist_manhattan.inv)$p.value
Moran.I(downscale_resids, loc.dist_manhattan.inv)$p.value
Moran.I(preds10m_resids, loc.dist_manhattan.inv)$p.value
Moran.I(preds5m_resids, loc.dist_manhattan.inv)$p.value

#all p values are very far from significant no matter the distance used