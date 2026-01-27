#perform the Mantel test, a nonparametric test for spatial correlation

rm(list=ls())
graphics.off()

setwd("/Users/f007f8t/Documents/GitHub/probDnsclRealData")

#setwd("/Users/f007f8t/Documents/GitHub/probDnsclRealData")

library(terra)
library(ggplot2)
library(ade4)

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

#compute distance matrices using euclidean distance for each type of residual and the observations
obs.dist_euclidean<- dist(obs,method="euclidean")
SLR_resids.dist_euclidean<- dist(SLR_resids,method="euclidean")
downscale_resids.dist_euclidean<- dist(downscale_resids,method="euclidean")
preds10m_resids.dist_euclidean<- dist(preds10m_resids,method="euclidean")
preds5m_resids.dist_euclidean<- dist(preds5m_resids,method="euclidean")

#compute distance matrix using euclidean distance for the locations
loc.dist_euclidean<- dist(HWMlocs,method="euclidean")

#perform the mantel test for spatial correlation using euclidean distance
mantel.obs.euclidean<- mantel.rtest(obs.dist_euclidean,loc.dist_euclidean)$pvalue
mantel.SLR_resids.euclidean<- mantel.rtest(SLR_resids.dist_euclidean,loc.dist_euclidean)$pvalue
mantel.downscale_resids.euclidean<- mantel.rtest(downscale_resids.dist_euclidean,loc.dist_euclidean)$pvalue
mantel.preds10m_resids.euclidean<- mantel.rtest(preds10m_resids.dist_euclidean,loc.dist_euclidean)$pvalue
mantel.preds5m_resids.euclidean<- mantel.rtest(preds5m_resids.dist_euclidean,loc.dist_euclidean)$pvalue

#compute distance matrices using manhattan distance for each type of residual and the observations
obs.dist_manhattan<- dist(obs,method="manhattan")
SLR_resids.dist_manhattan<- dist(SLR_resids,method="manhattan")
downscale_resids.dist_manhattan<- dist(downscale_resids,method="manhattan")
preds10m_resids.dist_manhattan<- dist(preds10m_resids,method="manhattan")
preds5m_resids.dist_manhattan<- dist(preds5m_resids,method="manhattan")

#compute distance matrix using manhattan distance for the locations
loc.dist_manhattan<- dist(HWMlocs,method="manhattan")

#perform the mantel test for spatial correlation using manhattan distance
mantel.obs.manhattan<- mantel.rtest(obs.dist_manhattan,loc.dist_manhattan)$pvalue
mantel.SLR_resids.manhattan<- mantel.rtest(SLR_resids.dist_manhattan,loc.dist_manhattan)$pvalue
mantel.downscale_resids.manhattan<- mantel.rtest(downscale_resids.dist_manhattan,loc.dist_manhattan)$pvalue
mantel.preds10m_resids.manhattan<- mantel.rtest(preds10m_resids.dist_manhattan,loc.dist_manhattan)$pvalue
mantel.preds5m_resids.manhattan<- mantel.rtest(preds5m_resids.dist_manhattan,loc.dist_manhattan)$pvalue


#nothing is even remotely near significant for any of the residuals or forms of distance considered