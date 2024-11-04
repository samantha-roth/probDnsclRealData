rm(list=ls())

library(terra)

pt<-proc.time()

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

#load the high water marks
load("data/HWMsdf.RData")
HWMlocs<- as.matrix(HWMs.df[,c("x","y")]); HWMlocs<- HWMlocs[1:5,]
obs<- HWMs.df$height[1:5]

#load the calibrated projections at each resolution
run10m<- rast("data/Outputs10m/Run_1.asc")

#load the DEM files at each resolution
dem5m<- rast("data/norristown_5m.asc")
dem10m<- rast("data/norristown_10m_new.asc")

################################################################################
#Next we use bilinear interpolation to get the WSH at 10m on 5m grid

load("data/coords.10m.RData")

min.x10<- min(coords.10m[,1])
min.y10<- min(coords.10m[,2])
max.x10<- max(coords.10m[,1])
max.y10<- max(coords.10m[,2])

goodLocs<- HWMlocs

y5m<- unique(goodLocs[,2])
x5m<- unique(goodLocs[,1])
y10m<- unique(coords.10m[,2])
x10m<- unique(coords.10m[,1])

nx5m<- length(x5m)
ny5m<- length(y5m)
nx10m<- length(x10m)
ny10m<- length(y10m)

library(akima)

elev.5m<- c(as.matrix(extract(dem5m,goodLocs)))
elev.10m<- c(as.matrix(extract(dem10m,coords.10m)))
wsh.10m<- c(as.matrix(extract(run10m,coords.10m)))
vals.10m<- wsh.10m + elev.10m

z1= matrix(vals.10m, nrow= ny10m, ncol= nx10m, byrow= TRUE)

z= matrix(NA,nrow= ny10m, ncol= nx10m)
for(j in 1:nx10m){ z[,j]<- rev(z1[,j]) }

test<- rep(NA, nrow(goodLocs))
for(i in 1:length(test)){
  test[i]<- bilinear(x= rev(y10m), y= x10m, z= z, 
                     x0= goodLocs[i,2], y0= goodLocs[i,1])$z
}

downscaled.z.vec<- test- elev.5m

downscale10m<- downscaled.z.vec

WSHtocompare<- obs

length(which(downscale10m<0))

downscale10m[which(downscale10m<0)]<- 0

save(downscale10m, file= "data/downscale10mto5mAtHWMs.RData")
#downscaling improves performance compared to just adjusting
#in terms of correlation and MAE but not MSE

ptFinal<-proc.time()-pt
time_downscale10mAtHWMs<-ptFinal[3]
save(time_downscale10mAtHWMs, file= "data/time_downscale10mto5mAtHWMs.RData")
