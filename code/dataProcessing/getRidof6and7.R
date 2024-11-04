#get best parameter settings for subsets of the locations

#find the parameter setting that best matches each HWM
#see if any HWMs have common parameter settings that match best

rm(list=ls())

library(terra)

setwd("/storage/work/svr5482")

if(dir.exists("probabilisticDownscaling/data/realObs/first5")==F){
  dir.create("probabilisticDownscaling/data/realObs/first5")}

#load HWMs
load("Reification/Philly/data/Manayunk/HWMsdf.RData")

HWMlocs<- as.matrix(HWMs.df[,c("x","y")])

nHWM<- nrow(HWMlocs)

#load parameter settings
load("FloodingModelCalibrationProject/parameterSamples/Philly/nCh_brkunif.RData")

nRuns<- length(c(as.matrix(samp10)))

################################################################################
#3m

allRunsHWMlocs.3m<- matrix(NA, nrow= nRuns, ncol= nHWM)
for(run in 1:nRuns){
  run3m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs3m/Norristown/nCh/Extent/Run_",run,".asc"))
  allRunsHWMlocs.3m[run,]<- c(as.matrix(extract(run3m,HWMlocs)))
}

MAE3m<- rep(NA, nRuns)
errors3m<- matrix(NA, nrow= nRuns, ncol= nHWM)
for(run in 1:nRuns){
  MAE3m[run]<- mean(abs(allRunsHWMlocs.3m[run,]-HWMs.df$height))
  errors3m[run,]<- allRunsHWMlocs.3m[run,]-HWMs.df$height
}

which.min(MAE3m)
MAE3m[which.min(MAE3m)]
errors3m[which.min(MAE3m),]
c(as.matrix(samp10))[which.min(MAE3m)] 

#what about if I get rid of 6 and 7

MAEno6and7.3m<- rep(NA, nRuns)
errorsno6and7.3m<- matrix(NA, nrow= nRuns, ncol= 5)
for(run in 1:nRuns){
  MAEno6and7.3m[run]<- mean(abs(allRunsHWMlocs.3m[run,1:5]-HWMs.df$height[1:5]))
  errorsno6and7.3m[run,]<- allRunsHWMlocs.3m[run,1:5]-HWMs.df$height[1:5]
}

which.min(MAEno6and7.3m)
MAEno6and7.3m[which.min(MAEno6and7.3m)]
errorsno6and7.3m[which.min(MAEno6and7.3m),]
c(as.matrix(samp10))[which.min(MAEno6and7.3m)] 

allRunsHWMlocs.3m<- allRunsHWMlocs.3m[,1:5]
save(allRunsHWMlocs.3m,file="probabilisticDownscaling/data/realObs/first5/allRunsHWMlocs.3m.RData")

################################################################################
#5m

allRunsHWMlocs.5m<- matrix(NA, nrow= nRuns, ncol= nHWM)
for(run in 1:nRuns){
  run5m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs5m/Norristown/nCh/Extent/Run_",run,".asc"))
  allRunsHWMlocs.5m[run,]<- c(as.matrix(extract(run5m,HWMlocs)))
}

MAE5m<- rep(NA, nRuns)
errors5m<- matrix(NA, nrow= nRuns, ncol= nHWM)
for(run in 1:nRuns){
  MAE5m[run]<- mean(abs(allRunsHWMlocs.5m[run,]-HWMs.df$height))
  errors5m[run,]<- allRunsHWMlocs.5m[run,]-HWMs.df$height
}

which.min(MAE5m)
MAE5m[which.min(MAE5m)]
errors5m[which.min(MAE5m),]
c(as.matrix(samp10))[which.min(MAE5m)] 

#what about if I get rid of 6 and 7

MAEno6and7.5m<- rep(NA, nRuns)
errorsno6and7.5m<- matrix(NA, nrow= nRuns, ncol= 5)
for(run in 1:nRuns){
  MAEno6and7.5m[run]<- mean(abs(allRunsHWMlocs.5m[run,1:5]-HWMs.df$height[1:5]))
  errorsno6and7.5m[run,]<- allRunsHWMlocs.5m[run,1:5]-HWMs.df$height[1:5]
}

which.min(MAEno6and7.5m)
MAEno6and7.5m[which.min(MAEno6and7.5m)]
errorsno6and7.5m[which.min(MAEno6and7.5m),]
c(as.matrix(samp10))[which.min(MAEno6and7.5m)] 

allRunsHWMlocs.5m<- allRunsHWMlocs.5m[,1:5]
save(allRunsHWMlocs.5m,file="probabilisticDownscaling/data/realObs/first5/allRunsHWMlocs.5m.RData")
################################################################################
#10m

allRunsHWMlocs.10m<- matrix(NA, nrow= nRuns, ncol= nHWM)
for(run in 1:nRuns){
  run10m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs10m/Norristown/nCh/Extent/Run_",run,".asc"))
  allRunsHWMlocs.10m[run,]<- c(as.matrix(extract(run10m,HWMlocs)))
}

MAE10m<- rep(NA, nRuns)
errors10m<- matrix(NA, nrow= nRuns, ncol= nHWM)
for(run in 1:nRuns){
  MAE10m[run]<- mean(abs(allRunsHWMlocs.10m[run,]-HWMs.df$height))
  errors10m[run,]<- allRunsHWMlocs.10m[run,]-HWMs.df$height
}

which.min(MAE10m)
MAE10m[which.min(MAE10m)]
errors10m[which.min(MAE10m),]
c(as.matrix(samp10))[which.min(MAE10m)] 

#what about if I get rid of 6 and 7

MAEno6and7.10m<- rep(NA, nRuns)
errorsno6and7.10m<- matrix(NA, nrow= nRuns, ncol= 5)
for(run in 1:nRuns){
  MAEno6and7.10m[run]<- mean(abs(allRunsHWMlocs.10m[run,1:5]-HWMs.df$height[1:5]))
  errorsno6and7.10m[run,]<- allRunsHWMlocs.10m[run,1:5]-HWMs.df$height[1:5]
}

which.min(MAEno6and7.10m)
MAEno6and7.10m[which.min(MAEno6and7.10m)]
errorsno6and7.10m[which.min(MAEno6and7.10m),]
c(as.matrix(samp10))[which.min(MAEno6and7.10m)] 


allRunsHWMlocs.10m<- allRunsHWMlocs.10m[,1:5]
save(allRunsHWMlocs.10m,file="probabilisticDownscaling/data/realObs/first5/allRunsHWMlocs.10m.RData")
################################################################################
#30m

allRunsHWMlocs.30m<- matrix(NA, nrow= nRuns, ncol= nHWM)
for(run in 1:nRuns){
  run30m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs30m/Norristown/nCh/Extent/Run_",run,".asc"))
  allRunsHWMlocs.30m[run,]<- c(as.matrix(extract(run30m,HWMlocs)))
}

MAE30m<- rep(NA, nRuns)
errors30m<- matrix(NA, nrow= nRuns, ncol= nHWM)
for(run in 1:nRuns){
  MAE30m[run]<- mean(abs(allRunsHWMlocs.30m[run,]-HWMs.df$height))
  errors30m[run,]<- allRunsHWMlocs.30m[run,]-HWMs.df$height
}

which.min(MAE30m)
MAE30m[which.min(MAE30m)]
errors30m[which.min(MAE30m),]
c(as.matrix(samp10))[which.min(MAE30m)] 

#what about if I get rid of 6 and 7

MAEno6and7.30m<- rep(NA, nRuns)
errorsno6and7.30m<- matrix(NA, nrow= nRuns, ncol= 5)
for(run in 1:nRuns){
  MAEno6and7.30m[run]<- mean(abs(allRunsHWMlocs.30m[run,1:5]-HWMs.df$height[1:5]))
  errorsno6and7.30m[run,]<- allRunsHWMlocs.30m[run,1:5]-HWMs.df$height[1:5]
}

which.min(MAEno6and7.30m)
MAEno6and7.30m[which.min(MAEno6and7.30m)]
errorsno6and7.30m[which.min(MAEno6and7.30m),]
c(as.matrix(samp10))[which.min(MAEno6and7.30m)] 


allRunsHWMlocs.30m<- allRunsHWMlocs.30m[,1:5]
save(allRunsHWMlocs.30m,file="probabilisticDownscaling/data/realObs/first5/allRunsHWMlocs.30m.RData")
################################################################################
#50m

allRunsHWMlocs.50m<- matrix(NA, nrow= nRuns, ncol= nHWM)
for(run in 1:nRuns){
  run50m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs50m/Norristown/nCh/Extent/Run_",run,".asc"))
  allRunsHWMlocs.50m[run,]<- c(as.matrix(extract(run50m,HWMlocs)))
}

MAE50m<- rep(NA, nRuns)
errors50m<- matrix(NA, nrow= nRuns, ncol= nHWM)
for(run in 1:nRuns){
  MAE50m[run]<- mean(abs(allRunsHWMlocs.50m[run,]-HWMs.df$height))
  errors50m[run,]<- allRunsHWMlocs.50m[run,]-HWMs.df$height
}

which.min(MAE50m)
MAE50m[which.min(MAE50m)]
errors50m[which.min(MAE50m),]
c(as.matrix(samp10))[which.min(MAE50m)] 

#what about if I get rid of 6 and 7

MAEno6and7.50m<- rep(NA, nRuns)
errorsno6and7.50m<- matrix(NA, nrow= nRuns, ncol= 5)
for(run in 1:nRuns){
  MAEno6and7.50m[run]<- mean(abs(allRunsHWMlocs.50m[run,1:5]-HWMs.df$height[1:5]))
  errorsno6and7.50m[run,]<- allRunsHWMlocs.50m[run,1:5]-HWMs.df$height[1:5]
}

which.min(MAEno6and7.50m)
MAEno6and7.50m[which.min(MAEno6and7.50m)]
errorsno6and7.50m[which.min(MAEno6and7.50m),]
c(as.matrix(samp10))[which.min(MAEno6and7.50m)] 

allRunsHWMlocs.50m<- allRunsHWMlocs.50m[,1:5]
save(allRunsHWMlocs.50m,file="probabilisticDownscaling/data/realObs/first5/allRunsHWMlocs.50m.RData")
ge
