rm(list=ls())

library(terra); library(DescTools)
setwd("C:/Users/svr5482")

#load files giving cost of getting from river to point considering elevation

dem5m<- rast("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/LISFLOOD/norristown_5m.asc")
dem30m<- rast("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/LISFLOOD/norristown_30m_new.asc")

load("probabilisticDownscaling/data/coords.30m.RData")

flow<- c("Q2559.8429","Q2503.2092","Q3681.19006")

################################################################################
#for(f in 1:length(flow)){
f=1  

pt<-proc.time()
#get coordinates of interest
run5m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs5m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
coords5m<- xyFromCell(run5m,1:ncell(run5m))

#load 5m locations that are within the bounds of the 10m, 30m rasters
load("probabilisticDownscaling/data/inds5min10m_30m.RData")

#load binary data showing with cells are flooded

floodBin30mat5m<- rast(paste0("probabilisticDownscaling/data/",flow[f],"/bin30mat5m.tif"))
floodBin5m<- rast(paste0("probabilisticDownscaling/data/",flow[f],"/bin5m.tif"))

################################################################################
elev30m<- c(as.matrix(extract(dem30m,coords.30m)))
elev30mat5m<- c(as.matrix(extract(dem30m,coords5m[goodInds5m,])))
elev5m<- c(as.matrix(extract(dem5m,coords5m[goodInds5m,])))

#now see if there's a relationship between elevation
#and whether or not the location is flooded

floodBin30mat5mVals<- c(as.matrix(extract(floodBin30mat5m,coords5m[goodInds5m,])))
floodBin5mVals<- c(as.matrix(extract(floodBin5m,coords5m[goodInds5m,])))

floodBin30mat5mVals<- ifelse(is.na(floodBin30mat5mVals),0,1)
floodBin5mVals<- ifelse(is.na(floodBin5mVals),0,1)

maxWetElev30m<- max(elev30mat5m[which(floodBin30mat5mVals==1)])
maxWetElev5m<- max(elev5m[which(floodBin5mVals==1)])
print(paste0("max wet elevation at 30m: ",maxWetElev30m))
print(paste0("max wet elevation at 5m: ",maxWetElev5m))

minDryElev30m<- min(elev30mat5m[which(floodBin30mat5mVals==0)])
minDryElev5m<- min(elev5m[which(floodBin5mVals==0)])
print(paste0("min dry elevation at 30m: ",minDryElev30m))
print(paste0("min dry elevation at 5m: ",minDryElev5m))

minNotFlooded<- min(elev30mat5m[which(floodBin30mat5mVals==0)])

maxFlooded<- max(elev30mat5m[which(floodBin30mat5mVals==1)])

##############################################################################

WetAndDryInds30m<- which(elev30mat5m>minDryElev30m & elev30mat5m<maxWetElev30m)
#hist(elev30mat5m[WetAndDryInds30m])
max(elev30mat5m[WetAndDryInds30m])
min(elev30mat5m[WetAndDryInds30m])
len<- (max(elev30mat5m[WetAndDryInds30m])-min(elev30mat5m[WetAndDryInds30m]))/15

divs<- seq(minDryElev30m,maxWetElev30m,by=len)
divs<- c(divs[1:2],divs[5],divs[7:length(divs)])

elevIntervals30m<- rep(NA, length(elev30mat5m))
elevIntervals5m<- rep(NA, length(elev5m))

performByDiv<- matrix(NA, nrow=length(divs)-1, ncol= 3)
colnames(performByDiv)<- c("LB","nLo,","nHi")

for(i in 1:(length(divs)-1)){
  performByDiv[i,1]<- divs[i]
  want30m<- which(elev30mat5m>=divs[i] & elev30mat5m<= divs[i+1])
  elevIntervals30m[want30m]<- divs[i]
  #print(paste0("# 30m cells: ", length(want30m)))
  want30mOGscale<- which(elev30m>=divs[i] & elev30m<= divs[i+1])
  performByDiv[i,2]<- length(want30mOGscale)
  
  want5m<- which(elev5m>=divs[i] & elev5m<= divs[i+1])
  elevIntervals5m[want5m]<- divs[i]
  #print(paste0("# 5m cells: ", length(want5m)))
  performByDiv[i,3]<- length(want5m)
}

pctFloodedbyElev30m<- rep(NA,length(divs)-1)
pctFloodedbyElev5m<- rep(NA,length(divs)-1)

for(i in 1: 1:(length(divs)-1)){
  if(length(which(elevIntervals5m==divs[i]))>0){
    pctFloodedbyElev5m[i]<- mean(floodBin5mVals[which(elevIntervals5m==divs[i])])}
  if(length(which(elevIntervals5m==divs[i]))==0) pctFloodedbyElev5m[i]<-0
  if(length(which(elevIntervals30m==divs[i]))>0){
    pctFloodedbyElev30m[i]<- mean(floodBin30mat5mVals[which(elevIntervals30m==divs[i])])}
  if(length(which(elevIntervals30m==divs[i]))==0) pctFloodedbyElev30m[i]<-0
}

print(paste0("MAE: ", mean(abs(pctFloodedbyElev30m-pctFloodedbyElev5m))))
print(paste0("Mean difference: ", mean(pctFloodedbyElev30m-pctFloodedbyElev5m)))

#midpts<- divs[1:(length(divs)-1)]+len/2
midpts<- divs[1:(length(divs)-1)]+ (divs[2:length(divs)]-divs[1:(length(divs)-1)])/2

filename<- paste0("probabilisticDownscaling/plots/",flow[f],"/ProbFloodbyElev30m.jpeg")
jpeg(file = filename,width = 700,height=700)
print(plot(midpts,pctFloodedbyElev30m,
           xlab="Approx. Elev",
           ylab="Probability of Being Flooded",
           main="Approx. Elev VS Probability of Flooding"))
print(points(midpts,pctFloodedbyElev5m,col="red"))
dev.off()

print(paste0("Difference > .05: ",midpts[which(abs(pctFloodedbyElev30m-pctFloodedbyElev5m)>.05)]))
print(paste0("Difference > .10: ",midpts[which(abs(pctFloodedbyElev30m-pctFloodedbyElev5m)>.1)]))
#across all resolutions, 
#given about the same cost of getting from the river to a point, 
#the probability of that point being flooded is about the same

LB<- divs[1:(length(divs)-1)]

save(pctFloodedbyElev30m,pctFloodedbyElev5m,len,divs,WetAndDryInds30m,
     minDryElev30m,maxWetElev30m,LB,midpts,
     file=paste0("probabilisticDownscaling/data/",flow[f],"/pctFloodedbyElevAndLB_30mto5m.RData"))

ptFinal<-proc.time()-pt
time_pctFloodedbyElev<-ptFinal[3]
save(time_pctFloodedbyElev, file= paste0("C:/Users/svr5482/probabilisticDownscaling/data/",flow[f],"/time_pctFloodedbyElev_30mto5m.RData"))

#}

################################################################################

#for(f in 1:length(flow)){
f=2  

pt<-proc.time()
#get coordinates of interest
run5m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs5m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
coords5m<- xyFromCell(run5m,1:ncell(run5m))

#load 5m locations that are within the bounds of the 10m, 30m rasters
load("probabilisticDownscaling/data/inds5min10m_30m.RData")

#load binary data showing with cells are flooded

floodBin30mat5m<- rast(paste0("probabilisticDownscaling/data/",flow[f],"/bin30mat5m.tif"))
floodBin5m<- rast(paste0("probabilisticDownscaling/data/",flow[f],"/bin5m.tif"))

################################################################################
elev30m<- c(as.matrix(extract(dem30m,coords.30m)))
elev30mat5m<- c(as.matrix(extract(dem30m,coords5m[goodInds5m,])))
elev5m<- c(as.matrix(extract(dem5m,coords5m[goodInds5m,])))

#now see if there's a relationship between elevation
#and whether or not the location is flooded

floodBin30mat5mVals<- c(as.matrix(extract(floodBin30mat5m,coords5m[goodInds5m,])))
floodBin5mVals<- c(as.matrix(extract(floodBin5m,coords5m[goodInds5m,])))

floodBin30mat5mVals<- ifelse(is.na(floodBin30mat5mVals),0,1)
floodBin5mVals<- ifelse(is.na(floodBin5mVals),0,1)

maxWetElev30m<- max(elev30mat5m[which(floodBin30mat5mVals==1)])
maxWetElev5m<- max(elev5m[which(floodBin5mVals==1)])
print(paste0("max wet elevation at 30m: ",maxWetElev30m))
print(paste0("max wet elevation at 5m: ",maxWetElev5m))

minDryElev30m<- min(elev30mat5m[which(floodBin30mat5mVals==0)])
minDryElev5m<- min(elev5m[which(floodBin5mVals==0)])
print(paste0("min dry elevation at 30m: ",minDryElev30m))
print(paste0("min dry elevation at 5m: ",minDryElev5m))

minNotFlooded<- min(elev30mat5m[which(floodBin30mat5mVals==0)])

maxFlooded<- max(elev30mat5m[which(floodBin30mat5mVals==1)])

##############################################################################

WetAndDryInds30m<- which(elev30mat5m>minDryElev30m & elev30mat5m<maxWetElev30m)
#hist(elev30mat5m[WetAndDryInds30m])
max(elev30mat5m[WetAndDryInds30m])
min(elev30mat5m[WetAndDryInds30m])
len<- (max(elev30mat5m[WetAndDryInds30m])-min(elev30mat5m[WetAndDryInds30m]))/15

divs<- seq(minDryElev30m,maxWetElev30m,by=len)
divs<- c(divs[1:2],divs[5],divs[7:length(divs)])

elevIntervals30m<- rep(NA, length(elev30mat5m))
elevIntervals5m<- rep(NA, length(elev5m))

performByDiv<- matrix(NA, nrow=length(divs)-1, ncol= 3)
colnames(performByDiv)<- c("LB","nLo,","nHi")

for(i in 1:(length(divs)-1)){
  performByDiv[i,1]<- divs[i]
  want30m<- which(elev30mat5m>=divs[i] & elev30mat5m<= divs[i+1])
  elevIntervals30m[want30m]<- divs[i]
  #print(paste0("# 30m cells: ", length(want30m)))
  want30mOGscale<- which(elev30m>=divs[i] & elev30m<= divs[i+1])
  performByDiv[i,2]<- length(want30mOGscale)
  
  want5m<- which(elev5m>=divs[i] & elev5m<= divs[i+1])
  elevIntervals5m[want5m]<- divs[i]
  print(paste0("# 5m cells: ", length(want5m)))
  performByDiv[i,3]<- length(want5m)
}

pctFloodedbyElev30m<- rep(NA,length(divs)-1)
pctFloodedbyElev5m<- rep(NA,length(divs)-1)

for(i in 1: 1:(length(divs)-1)){
  if(length(which(elevIntervals5m==divs[i]))>0){
    pctFloodedbyElev5m[i]<- mean(floodBin5mVals[which(elevIntervals5m==divs[i])])}
  if(length(which(elevIntervals5m==divs[i]))==0) pctFloodedbyElev5m[i]<-0
  if(length(which(elevIntervals30m==divs[i]))>0){
    pctFloodedbyElev30m[i]<- mean(floodBin30mat5mVals[which(elevIntervals30m==divs[i])])}
  if(length(which(elevIntervals30m==divs[i]))==0) pctFloodedbyElev30m[i]<-0
}

print(paste0("MAE: ", mean(abs(pctFloodedbyElev30m-pctFloodedbyElev5m))))
print(paste0("Mean difference: ", mean(pctFloodedbyElev30m-pctFloodedbyElev5m)))

#midpts<- divs[1:(length(divs)-1)]+len/2
midpts<- divs[1:(length(divs)-1)]+ (divs[2:length(divs)]-divs[1:(length(divs)-1)])/2

filename<- paste0("probabilisticDownscaling/plots/",flow[f],"/ProbFloodbyElev30m.jpeg")
jpeg(file = filename,width = 700,height=700)
print(plot(midpts,pctFloodedbyElev30m,
           xlab="Approx. Elev",
           ylab="Probability of Being Flooded",
           main="Approx. Elev VS Probability of Flooding"))
print(points(midpts,pctFloodedbyElev5m,col="red"))
dev.off()

print(paste0("Difference > .05: ",midpts[which(abs(pctFloodedbyElev30m-pctFloodedbyElev5m)>.05)]))
print(paste0("Difference > .10: ",midpts[which(abs(pctFloodedbyElev30m-pctFloodedbyElev5m)>.1)]))
#across all resolutions, 
#given about the same cost of getting from the river to a point, 
#the probability of that point being flooded is about the same

LB<- divs[1:(length(divs)-1)]

save(pctFloodedbyElev30m,pctFloodedbyElev5m,len,divs,WetAndDryInds30m,
     minDryElev30m,maxWetElev30m,LB,midpts,
     file=paste0("probabilisticDownscaling/data/",flow[f],"/pctFloodedbyElevAndLB_30mto5m.RData"))

ptFinal<-proc.time()-pt
time_pctFloodedbyElev<-ptFinal[3]
save(time_pctFloodedbyElev, file= paste0("C:/Users/svr5482/probabilisticDownscaling/data/",flow[f],"/time_pctFloodedbyElev_30mto5m.RData"))

#}

################################################################################

#for(f in 1:length(flow)){
f=3  

pt<-proc.time()
#get coordinates of interest
run5m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs5m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
coords5m<- xyFromCell(run5m,1:ncell(run5m))

#load 5m locations that are within the bounds of the 10m, 30m rasters
load("probabilisticDownscaling/data/inds5min10m_30m.RData")

#load binary data showing with cells are flooded

floodBin30mat5m<- rast(paste0("probabilisticDownscaling/data/",flow[f],"/bin30mat5m.tif"))
floodBin5m<- rast(paste0("probabilisticDownscaling/data/",flow[f],"/bin5m.tif"))

################################################################################
elev30m<- c(as.matrix(extract(dem30m,coords.30m)))
elev30mat5m<- c(as.matrix(extract(dem30m,coords5m[goodInds5m,])))
elev5m<- c(as.matrix(extract(dem5m,coords5m[goodInds5m,])))

#now see if there's a relationship between elevation
#and whether or not the location is flooded

floodBin30mat5mVals<- c(as.matrix(extract(floodBin30mat5m,coords5m[goodInds5m,])))
floodBin5mVals<- c(as.matrix(extract(floodBin5m,coords5m[goodInds5m,])))

floodBin30mat5mVals<- ifelse(is.na(floodBin30mat5mVals),0,1)
floodBin5mVals<- ifelse(is.na(floodBin5mVals),0,1)

maxWetElev30m<- max(elev30mat5m[which(floodBin30mat5mVals==1)])
maxWetElev5m<- max(elev5m[which(floodBin5mVals==1)])
print(paste0("max wet elevation at 30m: ",maxWetElev30m))
print(paste0("max wet elevation at 5m: ",maxWetElev5m))

minDryElev30m<- min(elev30mat5m[which(floodBin30mat5mVals==0)])
minDryElev5m<- min(elev5m[which(floodBin5mVals==0)])
print(paste0("min dry elevation at 30m: ",minDryElev30m))
print(paste0("min dry elevation at 5m: ",minDryElev5m))

minNotFlooded<- min(elev30mat5m[which(floodBin30mat5mVals==0)])

maxFlooded<- max(elev30mat5m[which(floodBin30mat5mVals==1)])

##############################################################################

WetAndDryInds30m<- which(elev30mat5m>minDryElev30m & elev30mat5m<maxWetElev30m)
#hist(elev30mat5m[WetAndDryInds30m])
max(elev30mat5m[WetAndDryInds30m])
min(elev30mat5m[WetAndDryInds30m])
len<- (max(elev30mat5m[WetAndDryInds30m])-min(elev30mat5m[WetAndDryInds30m]))/10

divs<- seq(minDryElev30m,maxWetElev30m,by=len)
#divs<- c(divs[1:11],divs[length(divs)])

elevIntervals30m<- rep(NA, length(elev30mat5m))
elevIntervals5m<- rep(NA, length(elev5m))

performByDiv<- matrix(NA, nrow=length(divs)-1, ncol= 3)
colnames(performByDiv)<- c("LB","nLo,","nHi")

for(i in 1:(length(divs)-1)){
  performByDiv[i,1]<- divs[i]
  want30m<- which(elev30mat5m>=divs[i] & elev30mat5m<= divs[i+1])
  elevIntervals30m[want30m]<- divs[i]
  #print(paste0("# 30m cells: ", length(want30m)))
  want30mOGscale<- which(elev30m>=divs[i] & elev30m<= divs[i+1])
  performByDiv[i,2]<- length(want30mOGscale)
  
  want5m<- which(elev5m>=divs[i] & elev5m<= divs[i+1])
  elevIntervals5m[want5m]<- divs[i]
  print(paste0("# 5m cells: ", length(want5m)))
  performByDiv[i,3]<- length(want5m)
}

pctFloodedbyElev30m<- rep(NA,length(divs)-1)
pctFloodedbyElev5m<- rep(NA,length(divs)-1)

for(i in 1: 1:(length(divs)-1)){
  if(length(which(elevIntervals5m==divs[i]))>0){
    pctFloodedbyElev5m[i]<- mean(floodBin5mVals[which(elevIntervals5m==divs[i])])}
  if(length(which(elevIntervals5m==divs[i]))==0) pctFloodedbyElev5m[i]<-0
  if(length(which(elevIntervals30m==divs[i]))>0){
    pctFloodedbyElev30m[i]<- mean(floodBin30mat5mVals[which(elevIntervals30m==divs[i])])}
  if(length(which(elevIntervals30m==divs[i]))==0) pctFloodedbyElev30m[i]<-0
}

print(paste0("MAE: ", mean(abs(pctFloodedbyElev30m-pctFloodedbyElev5m))))
print(paste0("Mean difference: ", mean(pctFloodedbyElev30m-pctFloodedbyElev5m)))

#midpts<- divs[1:(length(divs)-1)]+len/2
midpts<- divs[1:(length(divs)-1)]+ (divs[2:length(divs)]-divs[1:(length(divs)-1)])/2

filename<- paste0("probabilisticDownscaling/plots/",flow[f],"/ProbFloodbyElev30m.jpeg")
jpeg(file = filename,width = 700,height=700)
print(plot(midpts,pctFloodedbyElev30m,
           xlab="Approx. Elev",
           ylab="Probability of Being Flooded",
           main="Approx. Elev VS Probability of Flooding"))
print(points(midpts,pctFloodedbyElev5m,col="red"))
dev.off()

print(paste0("Difference > .05: ",midpts[which(abs(pctFloodedbyElev30m-pctFloodedbyElev5m)>.05)]))
print(paste0("Difference > .10: ",midpts[which(abs(pctFloodedbyElev30m-pctFloodedbyElev5m)>.1)]))
#across all resolutions, 
#given about the same cost of getting from the river to a point, 
#the probability of that point being flooded is about the same

LB<- divs[1:(length(divs)-1)]

save(pctFloodedbyElev30m,pctFloodedbyElev5m,len,divs,WetAndDryInds30m,
     minDryElev30m,maxWetElev30m,LB,midpts,
     file=paste0("probabilisticDownscaling/data/",flow[f],"/pctFloodedbyElevAndLB_30mto5m.RData"))

ptFinal<-proc.time()-pt
time_pctFloodedbyElev<-ptFinal[3]
save(time_pctFloodedbyElev, file= paste0("C:/Users/svr5482/probabilisticDownscaling/data/",flow[f],"/time_pctFloodedbyElev_30mto5m.RData"))

#}

################################################################################

