rm(list=ls())

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)
#print("Hi")
library(terra)

#get coordinates of interest

run5m<- rast("data/Outputs5m/Run_1.asc")
coords.5m<- xyFromCell(run5m,1:ncell(run5m))
save(coords.5m,file="data/coords.5m.RData")

run10m<- rast("data/Outputs10m/Run_1.asc")
coords.10m<- xyFromCell(run10m,1:ncell(run10m))
save(coords.10m,file="data/coords.10m.RData")

