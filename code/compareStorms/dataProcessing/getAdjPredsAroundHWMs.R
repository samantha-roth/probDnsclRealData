rm(list=ls())

library(terra)

#load the high water marks
load("C:/Users/svr5482/probDnsclRealData/data/HWMsdf.RData")
HWMlocs<- as.matrix(HWMs.df[,c("x","y")]); HWMlocs<- HWMlocs[1:5,]

flood<- c("flood2014","flood2020","floodfuture")

for(f in 1:length(flood)){
  
  #load the calibrated projections at each resolution
  setwd("C:/Users/svr5482/FloodingModelCalibrationProject/04-Spatial_Stats_Samantha")
  #run50m<- rast(paste0("Outputs50m/Norristown/nCh/PostMedCh/",flood[f],"/Extent/Run_1.asc"))
  #run30m<- rast(paste0("Outputs30m/Norristown/nCh/PostMedCh/",flood[f],"/Extent/Run_1.asc"))
  run10m<- rast(paste0("Outputs10m/Norristown/nCh/PostMedCh/",flood[f],"/Extent/Run_1.asc"))
  run5m<- rast(paste0("Outputs5m/Norristown/nCh/PostMedCh/",flood[f],"/Extent/Run_1.asc"))
  
  #load the DEM files at each resolution
  dem5m<- rast("C:/Users/svr5482/FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/LISFLOOD/norristown_5m.asc")
  dem10m<- rast("C:/Users/svr5482/FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/LISFLOOD/norristown_10m_new.asc")
  #dem30m<- rast("C:/Users/svr5482/FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/LISFLOOD/norristown_30m_new.asc")
  #dem50m<- rast("C:/Users/svr5482/FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/LISFLOOD/norristown_50m_new.asc")
  
  ################################################################################
  #get 30m, 10m flood predictions in a box around the 20 high water marks
  
  #plot(run30m)
  #points(x=c(HWMlocs[,1]),y=c(HWMlocs[,2]),col="red")
  
  load("C:/Users/svr5482/probDnsclRealData/data/boxAroundHWMs.5m.RData")
  
  #get the lower resolution projections at the right locations
  #vals50minBds<- c(as.matrix(extract(run50m,coordsinBds.5m)))
  #vals30minBds<- c(as.matrix(extract(run30m,coordsinBds.5m)))
  vals10minBds<- c(as.matrix(extract(run10m,coordsinBds.5m)))
  vals5minBds<- c(as.matrix(extract(run5m,coordsinBds.5m)))
  
  #get lower resolution elevations at the right locations
  #elev50minBds<- c(as.matrix(extract(dem50m,coordsinBds.5m)))
  #elev30minBds<- c(as.matrix(extract(dem30m,coordsinBds.5m)))
  elev10minBds<- c(as.matrix(extract(dem10m,coordsinBds.5m)))
  elev5minBds<- c(as.matrix(extract(dem5m,coordsinBds.5m)))
  
  #adjust the values using the 5m DEM
  #vals50minBdsAdj<- vals50minBds+elev50minBds-elev5minBds
  #vals30minBdsAdj<- vals30minBds+elev30minBds-elev5minBds
  vals10minBdsAdj<- vals10minBds+elev10minBds-elev5minBds
  
  #vals50minBdsAdj[which(vals50minBdsAdj<0)]<- 0
  #vals30minBdsAdj[which(vals30minBdsAdj<0)]<- 0
  vals10minBdsAdj[which(vals10minBdsAdj<0)]<- 0
  
  #save(vals50minBdsAdj,file=paste0("C:/Users/svr5482/probDnsclRealData/data/",flood[f],"/vals50mto5minBdsAroundHWMsAdj.RData"))
  #save(vals30minBdsAdj,file=paste0("C:/Users/svr5482/probDnsclRealData/data/",flood[f],"/vals30mto5minBdsAroundHWMsAdj.RData"))
  save(vals10minBdsAdj,file=paste0("C:/Users/svr5482/probDnsclRealData/data/",flood[f],"/vals10mto5minBdsAroundHWMsAdj.RData"))
  save(vals5minBds,file=paste0("C:/Users/svr5482/probDnsclRealData/data/",flood[f],"/vals5minBdsAroundHWMs.RData"))
  
  ################################################################################
  ################################################################################
  #get adjusted 50m, 30m, 10m flood predictions at the 20 high water marks
  
  #vals50m<- c(as.matrix(extract(run50m,HWMlocs)))
  #vals30m<- c(as.matrix(extract(run30m,HWMlocs)))
  vals10m<- c(as.matrix(extract(run10m,HWMlocs)))
  vals5m<- c(as.matrix(extract(run5m,HWMlocs)))
  
  #elev50m<- c(as.matrix(extract(dem50m,HWMlocs)))
  #elev30m<- c(as.matrix(extract(dem30m,HWMlocs)))
  elev10m<- c(as.matrix(extract(dem10m,HWMlocs)))
  elev5m<- c(as.matrix(extract(dem5m,HWMlocs)))
  
  #adjust the values using the 5m DEM
  #vals50mAdj<- vals50m+elev50m-elev5m
  #vals30mAdj<- vals30m+elev30m-elev5m
  vals10mAdj<- vals10m+elev10m-elev5m
  
  #vals50mAdj[which(vals50mAdj<0)]<- 0
  #vals30mAdj[which(vals30mAdj<0)]<- 0
  vals10mAdj[which(vals10mAdj<0)]<- 0
  
  #save(vals50mAdj,file=paste0("C:/Users/svr5482/probDnsclRealData/data/",flood[f],"/vals50mto5mAtHWMsAdj.RData"))
  #save(vals30mAdj,file=paste0("C:/Users/svr5482/probDnsclRealData/data/",flood[f],"/vals30mto5mAtHWMsAdj.RData"))
  save(vals10mAdj,file=paste0("C:/Users/svr5482/probDnsclRealData/data/",flood[f],"/vals10mto5mAtHWMsAdj.RData"))
  save(vals5m,file=paste0("C:/Users/svr5482/probDnsclRealData/data/",flood[f],"/vals5mAtHWMs.RData"))
  
}
