
rm(list=ls())

library(terra); library(DescTools)
setwd("C:/Users/svr5482")

pt<-proc.time()

#load files giving cost of getting from river to point considering elevation

costbyElev_River5m<- rast("Documents/ArcGIS/Projects/Norristown_HurricaneIda/CostDis_riv5_1.tif")
costbyElev_River30m<- rast("Documents/ArcGIS/Projects/Norristown_HurricaneIda/CostDis_riv30_1.tif")

coords5m_cost<- xyFromCell(costbyElev_River5m,1:ncell(costbyElev_River5m))

#plot(costbyElev_River30m)
#plot(costbyElev_River5m)

flow<- c("Q2559.8429","Q2503.2092","Q3681.19006")

for(f in 1:length(flow)){
  
  #get coordinates of interest
  run5m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs5m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  coords5m<- xyFromCell(run5m,1:ncell(run5m))
  
  #load 5m locations that are within the bounds of the 10m, 30m rasters
  load("probabilisticDownscaling/data/inds5min10m_30m.RData")
  
  #load binary data showing with cells are flooded
  
  floodBin30mat5m<- rast(paste0("probabilisticDownscaling/data/",flow[f],"/bin30mat5m.tif"))
  floodBin5m<- rast(paste0("probabilisticDownscaling/data/",flow[f],"/bin5m.tif"))
  
  ################################################################################
  
  
  #length(which(coords5m_cost[,1]!=coords5m[,1]))
  #length(which(coords5m_cost[,2]!=coords5m[,2]))
  
  #max(abs(coords5m_cost[,1]-coords5m[,1]))
  #max(abs(coords5m_cost[,2]-coords5m[,2])) 
  #not big enough differences to worry about relative to the scale of the x and y values
  
  #now that we've established the 5m points where 30m and 30m cost values are defined
  #compare cost values at these points across resolutions
  #and get the percent of the time points with cost values within different ranges are flooded
  
  costbyElev_River30mat5mVals<- extract(costbyElev_River30m,coords5m[goodInds5m,])
  costbyElev_River5mVals<- extract(costbyElev_River5m,coords5m[goodInds5m,])
  
  costbyElev_River30mat5mVals<- c(as.matrix(costbyElev_River30mat5mVals))
  costbyElev_River5mVals<- c(as.matrix(costbyElev_River5mVals))
  
  #now see if there's a relationship between cost of getting 
  #from the river to the destination considering location 
  #and whether or not the location is flooded
  
  floodBin30mat5mVals<- extract(floodBin30mat5m,coords5m[goodInds5m,])
  floodBin5mVals<- extract(floodBin5m,coords5m[goodInds5m,])
  
  floodBin30mat5mVals<- c(as.matrix(floodBin30mat5mVals))
  floodBin5mVals<- c(as.matrix(floodBin5mVals))
  
  floodBin30mat5mVals<- ifelse(is.na(floodBin30mat5mVals),0,1)
  floodBin5mVals<- ifelse(is.na(floodBin5mVals),0,1)
  
  print(max(costbyElev_River30mat5mVals[which(floodBin30mat5mVals==1)]))
  print(max(costbyElev_River5mVals[which(floodBin5mVals==1)]))
  
  max(costbyElev_River30mat5mVals); max(costbyElev_River5mVals)
  length(which(costbyElev_River30mat5mVals>16000))
  
  len=500
  roundlen<- RoundTo(max(costbyElev_River30mat5mVals),len)
  if(max(costbyElev_River30mat5mVals)>roundlen) topcost<- roundlen+len
  if(max(costbyElev_River30mat5mVals)<=roundlen) topcost<- roundlen
  
  #now if as we get closer to 30000, see if the percentage of flooded cells decreases
  
  divs<- seq(0,topcost,by=len)
  
  costIntervals30m<- rep(NA, length(costbyElev_River30mat5mVals))
  costIntervals5m<- rep(NA, length(costbyElev_River5m))
  
  performByDiv<- matrix(NA, nrow=length(divs)-1, ncol= 3)
  colnames(performByDiv)<- c("LB","nLo,","nHi")
  
  for(i in 1:(length(divs)-1)){
    performByDiv[i,1]<- divs[i]
    want30m<- which((costbyElev_River30mat5mVals>=divs[i]& costbyElev_River30mat5mVals<= divs[i+1])==T)
    costIntervals30m[want30m]<- divs[i]
    #print(paste0("# 30m cells: ", length(want30m)))
    performByDiv[i,2]<- length(want30m)
    
    want5m<- which((costbyElev_River5mVals>=divs[i]& costbyElev_River5mVals<= divs[i+1])==T)
    costIntervals5m[want5m]<- divs[i]
    #print(paste0("# 5m cells: ", length(want5m)))
    performByDiv[i,3]<- length(want5m)
  }
  
  pctFloodedbyCost30m<- rep(NA,length(divs)-1)
  pctFloodedbyCost5m<- rep(NA,length(divs)-1)
  
  for(i in 1: 1:(length(divs)-1)){
    if(length(which(costIntervals5m==divs[i]))>0){
      pctFloodedbyCost5m[i]<- mean(floodBin5mVals[which(costIntervals5m==divs[i])])}
    if(length(which(costIntervals5m==divs[i]))==0) pctFloodedbyCost5m[i]<-0
    if(length(which(costIntervals30m==divs[i]))>0){
      pctFloodedbyCost30m[i]<- mean(floodBin30mat5mVals[which(costIntervals30m==divs[i])])}
    if(length(which(costIntervals30m==divs[i]))==0) pctFloodedbyCost30m[i]<-0
  }
  
  #plot(abs(performByDiv[,2]-performByDiv[,3]),abs(pctFloodedbyCost30m-pctFloodedbyCost5m))
  print(mean(abs(pctFloodedbyCost30m-pctFloodedbyCost5m)))
  print(mean(pctFloodedbyCost30m-pctFloodedbyCost5m))
  
  midpts<- divs[1:(length(divs)-1)]+len/2
  
  print(plot(midpts,pctFloodedbyCost30m,
             xlab="Approx. Cost from River to Point",
             ylab="Probability of Being Flooded",
             main="Approx. Cost from River VS Probability of Flooding"))
  print(points(midpts,pctFloodedbyCost5m,col="red"))
  
  
  print(midpts[which(abs(pctFloodedbyCost30m-pctFloodedbyCost5m)>.05)])
  #across all resolutions, 
  #given about the same cost of getting from the river to a point, 
  #the probability of that point being flooded is about the same
  
  LB<- divs[1:(length(divs)-1)]
  #save(pctFloodedbyCost30m,pctFloodedbyCost30m,pctFloodedbyCost5m,LB,
  #     file="probabilisticDownscaling/data/pctFloodedbyCostAndLB.RData")
  
  save(pctFloodedbyCost30m,pctFloodedbyCost5m,LB,
       file=paste0("probabilisticDownscaling/data/",flow[f],"/pctFloodedbyCostAndLB_30mto5m.RData"))
  
  ptFinal<-proc.time()-pt
  time_pctFloodedbyCost<-ptFinal[3]
  save(time_pctFloodedbyCost, file= paste0("C:/Users/svr5482/probabilisticDownscaling/data/",flow[f],"/time_pctFloodedbyCost_30mto5m.RData"))
  
}

