
rm(list=ls())
graphics.off()

library(terra)
library(nimble)
library(MCMCvis)

setwd("/Users/f007f8t/Documents/GitHub/probDnsclRealData")

run10m<- rast("data/Outputs10m/Run_1.asc")

load("data/coords.5m.RData")
load("data/coords.10m.RData")

load("/Users/f007f8t/svr5482/Reification/Philly/data/Norristown/nCh/not.river.inds10m.RData")
load("/Users/f007f8t/svr5482/Reification/Philly/data/Norristown/nCh/below.river.inds10m.RData")
load("/Users/f007f8t/svr5482/Reification/Philly/data/Norristown/nCh/above.river.inds10m.RData")

inds.10m<- 1:nrow(coords.10m)
river.inds<- inds.10m[-not.river.inds10m]

#now find the intersections of above, below, and within the river with our area of interest
load("data/boxAroundHWMs.10m.RData")

goodinds_aboveriver<- intersect(good10minds,above.river.inds)
goodinds_belowriver<- intersect(good10minds,below.river.inds)
goodinds_inriver<- intersect(good10minds,river.inds)


river_above_below<- run10m
terra::values(river_above_below)<- 0
terra::values(river_above_below)[goodinds_aboveriver]<-100
terra::values(river_above_below)[goodinds_inriver]<-50
terra::values(river_above_below)[goodinds_belowriver]<-75
plot(river_above_below)

river_above_below_vals<- terra::values(river_above_below)

x_max_inbds<- max(coordsinBds.10m[,"x"])


uh_oh.inds<- intersect(which(coords.10m[,"x"]<=x_max_inbds),which(river_above_below_vals==0))

river.inds<- c(river.inds,uh_oh.inds)
goodinds_inriver<- intersect(good10minds,river.inds)

river_above_below<- run10m
terra::values(river_above_below)<- 0
terra::values(river_above_below)[goodinds_aboveriver]<-100
terra::values(river_above_below)[goodinds_inriver]<-50
terra::values(river_above_below)[goodinds_belowriver]<-75
plot(river_above_below)

save(goodinds_aboveriver,file="data/goodinds_aboveriver")
save(goodinds_aboveriver,file="data/goodinds_belowriver")
save(goodinds_inriver,file="data/goodinds_inriver")