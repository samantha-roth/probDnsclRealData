rm(list=ls())

library(terra); library(DescTools)

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

#load files giving cost of getting from river to point considering elevation

dem5m<- rast("data/norristown_5m.asc")
dem10m<- rast("data/norristown_10m_new.asc")

load("data/coords.10m.RData")
load("data/coords.5m.RData")

#determine 5m locations that are within the bounds of the 10m raster
x.min.10m<- min(coords.10m[,"x"]); x.max.10m<- max(coords.10m[,"x"])
y.min.10m<- min(coords.10m[,"y"]); y.max.10m<- max(coords.10m[,"y"])

x.min.5m<- min(coords.5m[,"x"]); x.max.5m<- max(coords.5m[,"x"])
y.min.5m<- min(coords.5m[,"y"]); y.max.5m<- max(coords.5m[,"y"])

xInBds<- intersect(which(coords.5m[,"x"]>x.min.10m),which(coords.5m[,"x"]<x.max.10m))
yInBds<- intersect(which(coords.5m[,"y"]>y.min.10m),which(coords.5m[,"y"]<y.max.10m))
goodInds5m<- intersect(xInBds,yInBds)

flood<- c("flood2014","flood2020","floodfuture")

################################################################################

pt<-proc.time()

f=1  

if(dir.exists(paste0("plots/",flood[f]))==FALSE){
  dir.create(paste0("plots/",flood[f]))}

#get coordinates of interest
run5m<- rast(paste0("data/Outputs5m/",flood[f],"/Run_1.asc"))
run10m<- rast(paste0("data/Outputs10m/",flood[f],"/Run_1.asc"))

#load binary data showing with cells are flooded

floodBin10mat5m<- rast(paste0("data/",flood[f],"/bin10mat5m.tif"))
floodBin5m<- rast(paste0("data/",flood[f],"/bin5m.tif"))

################################################################################
elev10m<- c(as.matrix(extract(dem10m,coords.10m)))
elev10mat5m<- c(as.matrix(extract(dem10m,coords.5m[goodInds5m,])))
elev5m<- c(as.matrix(extract(dem5m,coords.5m[goodInds5m,])))

#now see if there's a relationship between elevation
#and whether or not the location is flooded

floodBin10mat5mVals<- c(as.matrix(extract(floodBin10mat5m,coords.5m[goodInds5m,])))
floodBin5mVals<- c(as.matrix(extract(floodBin5m,coords.5m[goodInds5m,])))

floodBin10mat5mVals<- ifelse(is.na(floodBin10mat5mVals),0,1)
floodBin5mVals<- ifelse(is.na(floodBin5mVals),0,1)

maxWetElev10m<- max(elev10mat5m[which(floodBin10mat5mVals==1)])
maxWetElev5m<- max(elev5m[which(floodBin5mVals==1)])
print(paste0("max wet elevation at 10m: ",maxWetElev10m))
print(paste0("max wet elevation at 5m: ",maxWetElev5m))

minDryElev10m<- min(elev10mat5m[which(floodBin10mat5mVals==0)])
minDryElev5m<- min(elev5m[which(floodBin5mVals==0)])
print(paste0("min dry elevation at 10m: ",minDryElev10m))
print(paste0("min dry elevation at 5m: ",minDryElev5m))

minNotFlooded<- min(elev10mat5m[which(floodBin10mat5mVals==0)])

maxFlooded<- max(elev10mat5m[which(floodBin10mat5mVals==1)])

##############################################################################

WetAndDryInds10m<- which(elev10mat5m>minDryElev10m & elev10mat5m<maxWetElev10m)
#hist(elev10mat5m[WetAndDryInds10m])
max(elev10mat5m[WetAndDryInds10m])
min(elev10mat5m[WetAndDryInds10m])
len<- (max(elev10mat5m[WetAndDryInds10m])-min(elev10mat5m[WetAndDryInds10m]))/8

divs<- seq(minDryElev10m,maxWetElev10m,by=len)
#divs<- c(divs[1:2],divs[5],divs[7:length(divs)])

elevIntervals10m<- rep(NA, length(elev10mat5m))
elevIntervals5m<- rep(NA, length(elev5m))

performByDiv<- matrix(NA, nrow=length(divs)-1, ncol= 3)
colnames(performByDiv)<- c("LB","nLo,","nHi")

for(i in 1:(length(divs)-1)){
  performByDiv[i,1]<- divs[i]
  want10m<- which(elev10mat5m>=divs[i] & elev10mat5m<= divs[i+1])
  elevIntervals10m[want10m]<- divs[i]
  
  want10mOGres<- which(elev10m>=divs[i] & elev10m<= divs[i+1])
  print(paste0("# 10m cells: ", length(want10mOGres)))
  performByDiv[i,2]<- length(want10mOGres)
  
  want5m<- which(elev5m>=divs[i] & elev5m<= divs[i+1])
  elevIntervals5m[want5m]<- divs[i]
  print(paste0("# 5m cells: ", length(want5m)))
  performByDiv[i,3]<- length(want5m)
}

pctFloodedbyElev10m<- rep(NA,length(divs)-1)
pctFloodedbyElev5m<- rep(NA,length(divs)-1)

for(i in 1: 1:(length(divs)-1)){
  if(length(which(elevIntervals5m==divs[i]))>0){
    pctFloodedbyElev5m[i]<- mean(floodBin5mVals[which(elevIntervals5m==divs[i])])}
  if(length(which(elevIntervals5m==divs[i]))==0) pctFloodedbyElev5m[i]<-0
  if(length(which(elevIntervals10m==divs[i]))>0){
    pctFloodedbyElev10m[i]<- mean(floodBin10mat5mVals[which(elevIntervals10m==divs[i])])}
  if(length(which(elevIntervals10m==divs[i]))==0) pctFloodedbyElev10m[i]<-0
}

print(paste0("MAE: ", mean(abs(pctFloodedbyElev10m-pctFloodedbyElev5m))))
print(paste0("Mean difference: ", mean(pctFloodedbyElev10m-pctFloodedbyElev5m)))

#midpts<- divs[1:(length(divs)-1)]+len/2
midpts<- divs[1:(length(divs)-1)]+ (divs[2:length(divs)]-divs[1:(length(divs)-1)])/2

filename<- paste0("plots/",flood[f],"/ProbFloodbyElev10m.jpeg")
jpeg(file = filename,width = 700,height=700)
print(plot(midpts,pctFloodedbyElev10m,
           xlab="Approx. Elev",
           ylab="Probability of Being Flooded",
           main="Approx. Elev VS Probability of Flooding"))
print(points(midpts,pctFloodedbyElev5m,col="red"))
dev.off()

print(paste0("Difference > .05: ",midpts[which(abs(pctFloodedbyElev10m-pctFloodedbyElev5m)>.05)]))
print(paste0("Difference > .10: ",midpts[which(abs(pctFloodedbyElev10m-pctFloodedbyElev5m)>.1)]))
#across all resolutions, 
#given about the same cost of getting from the river to a point, 
#the probability of that point being flooded is about the same

LB<- divs[1:(length(divs)-1)]

save(pctFloodedbyElev10m,pctFloodedbyElev5m,len,divs,WetAndDryInds10m,
     minDryElev10m,maxWetElev10m,LB,midpts,
     file=paste0("data/",flood[f],"/pctFloodedbyElevAndLB_10mto5m.RData"))

ptFinal<-proc.time()-pt
time_pctFloodedbyElev<-ptFinal[3]
save(time_pctFloodedbyElev, file= paste0("data/",flood[f],"/time_pctFloodedbyElev_10mto5m.RData"))

#}

################################################################################
pt<-proc.time()

f=2  

if(dir.exists(paste0("plots/",flood[f]))==FALSE){
  dir.create(paste0("plots/",flood[f]))}

#get coordinates of interest
run5m<- rast(paste0("data/Outputs5m/",flood[f],"/Run_1.asc"))
run10m<- rast(paste0("data/Outputs10m/",flood[f],"/Run_1.asc"))

#load binary data showing with cells are flooded

floodBin10mat5m<- rast(paste0("data/",flood[f],"/bin10mat5m.tif"))
floodBin5m<- rast(paste0("data/",flood[f],"/bin5m.tif"))

################################################################################
elev10m<- c(as.matrix(extract(dem10m,coords.10m)))
elev10mat5m<- c(as.matrix(extract(dem10m,coords.5m[goodInds5m,])))
elev5m<- c(as.matrix(extract(dem5m,coords.5m[goodInds5m,])))

#now see if there's a relationship between elevation
#and whether or not the location is flooded

floodBin10mat5mVals<- c(as.matrix(extract(floodBin10mat5m,coords.5m[goodInds5m,])))
floodBin5mVals<- c(as.matrix(extract(floodBin5m,coords.5m[goodInds5m,])))

floodBin10mat5mVals<- ifelse(is.na(floodBin10mat5mVals),0,1)
floodBin5mVals<- ifelse(is.na(floodBin5mVals),0,1)

maxWetElev10m<- max(elev10mat5m[which(floodBin10mat5mVals==1)])
maxWetElev5m<- max(elev5m[which(floodBin5mVals==1)])
print(paste0("max wet elevation at 10m: ",maxWetElev10m))
print(paste0("max wet elevation at 5m: ",maxWetElev5m))

minDryElev10m<- min(elev10mat5m[which(floodBin10mat5mVals==0)])
minDryElev5m<- min(elev5m[which(floodBin5mVals==0)])
print(paste0("min dry elevation at 10m: ",minDryElev10m))
print(paste0("min dry elevation at 5m: ",minDryElev5m))

minNotFlooded<- min(elev10mat5m[which(floodBin10mat5mVals==0)])

maxFlooded<- max(elev10mat5m[which(floodBin10mat5mVals==1)])

##############################################################################

WetAndDryInds10m<- which(elev10mat5m>minDryElev10m & elev10mat5m<maxWetElev10m)
#hist(elev10mat5m[WetAndDryInds10m])
max(elev10mat5m[WetAndDryInds10m])
min(elev10mat5m[WetAndDryInds10m])
len<- (max(elev10mat5m[WetAndDryInds10m])-min(elev10mat5m[WetAndDryInds10m]))/8

divs<- seq(minDryElev10m,maxWetElev10m,by=len)
#divs<- c(divs[1:2],divs[5],divs[7:length(divs)])

elevIntervals10m<- rep(NA, length(elev10mat5m))
elevIntervals5m<- rep(NA, length(elev5m))

performByDiv<- matrix(NA, nrow=length(divs)-1, ncol= 3)
colnames(performByDiv)<- c("LB","nLo,","nHi")

for(i in 1:(length(divs)-1)){
  performByDiv[i,1]<- divs[i]
  want10m<- which(elev10mat5m>=divs[i] & elev10mat5m<= divs[i+1])
  elevIntervals10m[want10m]<- divs[i]
  #print(paste0("# 10m cells: ", length(want10m)))
  want10mOGscale<- which(elev10m>=divs[i] & elev10m<= divs[i+1])
  performByDiv[i,2]<- length(want10mOGscale)
  
  want5m<- which(elev5m>=divs[i] & elev5m<= divs[i+1])
  elevIntervals5m[want5m]<- divs[i]
  print(paste0("# 5m cells: ", length(want5m)))
  performByDiv[i,3]<- length(want5m)
}

pctFloodedbyElev10m<- rep(NA,length(divs)-1)
pctFloodedbyElev5m<- rep(NA,length(divs)-1)

for(i in 1: 1:(length(divs)-1)){
  if(length(which(elevIntervals5m==divs[i]))>0){
    pctFloodedbyElev5m[i]<- mean(floodBin5mVals[which(elevIntervals5m==divs[i])])}
  if(length(which(elevIntervals5m==divs[i]))==0) pctFloodedbyElev5m[i]<-0
  if(length(which(elevIntervals10m==divs[i]))>0){
    pctFloodedbyElev10m[i]<- mean(floodBin10mat5mVals[which(elevIntervals10m==divs[i])])}
  if(length(which(elevIntervals10m==divs[i]))==0) pctFloodedbyElev10m[i]<-0
}

print(paste0("MAE: ", mean(abs(pctFloodedbyElev10m-pctFloodedbyElev5m))))
print(paste0("Mean difference: ", mean(pctFloodedbyElev10m-pctFloodedbyElev5m)))

#midpts<- divs[1:(length(divs)-1)]+len/2
midpts<- divs[1:(length(divs)-1)]+ (divs[2:length(divs)]-divs[1:(length(divs)-1)])/2

filename<- paste0("plots/",flood[f],"/ProbFloodbyElev10m.jpeg")
jpeg(file = filename,width = 700,height=700)
print(plot(midpts,pctFloodedbyElev10m,
           xlab="Approx. Elev",
           ylab="Probability of Being Flooded",
           main="Approx. Elev VS Probability of Flooding"))
print(points(midpts,pctFloodedbyElev5m,col="red"))
dev.off()

print(paste0("Difference > .05: ",midpts[which(abs(pctFloodedbyElev10m-pctFloodedbyElev5m)>.05)]))
print(paste0("Difference > .10: ",midpts[which(abs(pctFloodedbyElev10m-pctFloodedbyElev5m)>.1)]))
#across all resolutions, 
#given about the same cost of getting from the river to a point, 
#the probability of that point being flooded is about the same

LB<- divs[1:(length(divs)-1)]

save(pctFloodedbyElev10m,pctFloodedbyElev5m,len,divs,WetAndDryInds10m,
     minDryElev10m,maxWetElev10m,LB,midpts,
     file=paste0("data/",flood[f],"/pctFloodedbyElevAndLB_10mto5m.RData"))

ptFinal<-proc.time()-pt
time_pctFloodedbyElev<-ptFinal[3]
save(time_pctFloodedbyElev, file= paste0("data/",flood[f],"/time_pctFloodedbyElev_10mto5m.RData"))



################################################################################
pt<-proc.time()

f=3  

if(dir.exists(paste0("plots/",flood[f]))==FALSE){
  dir.create(paste0("plots/",flood[f]))}

#get coordinates of interest
run5m<- rast(paste0("data/Outputs5m/",flood[f],"/Run_1.asc"))
run10m<- rast(paste0("data/Outputs10m/",flood[f],"/Run_1.asc"))

#load binary data showing with cells are flooded

floodBin10mat5m<- rast(paste0("data/",flood[f],"/bin10mat5m.tif"))
floodBin5m<- rast(paste0("data/",flood[f],"/bin5m.tif"))

################################################################################
elev10m<- c(as.matrix(extract(dem10m,coords.10m)))
elev10mat5m<- c(as.matrix(extract(dem10m,coords.5m[goodInds5m,])))
elev5m<- c(as.matrix(extract(dem5m,coords.5m[goodInds5m,])))

#now see if there's a relationship between elevation
#and whether or not the location is flooded

floodBin10mat5mVals<- c(as.matrix(extract(floodBin10mat5m,coords.5m[goodInds5m,])))
floodBin5mVals<- c(as.matrix(extract(floodBin5m,coords.5m[goodInds5m,])))

floodBin10mat5mVals<- ifelse(is.na(floodBin10mat5mVals),0,1)
floodBin5mVals<- ifelse(is.na(floodBin5mVals),0,1)

maxWetElev10m<- max(elev10mat5m[which(floodBin10mat5mVals==1)])
maxWetElev5m<- max(elev5m[which(floodBin5mVals==1)])
print(paste0("max wet elevation at 10m: ",maxWetElev10m))
print(paste0("max wet elevation at 5m: ",maxWetElev5m))

minDryElev10m<- min(elev10mat5m[which(floodBin10mat5mVals==0)])
minDryElev5m<- min(elev5m[which(floodBin5mVals==0)])
print(paste0("min dry elevation at 10m: ",minDryElev10m))
print(paste0("min dry elevation at 5m: ",minDryElev5m))

minNotFlooded<- min(elev10mat5m[which(floodBin10mat5mVals==0)])

maxFlooded<- max(elev10mat5m[which(floodBin10mat5mVals==1)])

##############################################################################

WetAndDryInds10m<- which(elev10mat5m>minDryElev10m & elev10mat5m<maxWetElev10m)
#hist(elev10mat5m[WetAndDryInds10m])
max(elev10mat5m[WetAndDryInds10m])
min(elev10mat5m[WetAndDryInds10m])
len<- (max(elev10mat5m[WetAndDryInds10m])-min(elev10mat5m[WetAndDryInds10m]))/8

divs<- seq(minDryElev10m,maxWetElev10m,by=len)
#divs<- c(divs[1:11],divs[length(divs)])

elevIntervals10m<- rep(NA, length(elev10mat5m))
elevIntervals5m<- rep(NA, length(elev5m))

performByDiv<- matrix(NA, nrow=length(divs)-1, ncol= 3)
colnames(performByDiv)<- c("LB","nLo,","nHi")

for(i in 1:(length(divs)-1)){
  performByDiv[i,1]<- divs[i]
  want10m<- which(elev10mat5m>=divs[i] & elev10mat5m<= divs[i+1])
  elevIntervals10m[want10m]<- divs[i]
  #print(paste0("# 10m cells: ", length(want10m)))
  want10mOGscale<- which(elev10m>=divs[i] & elev10m<= divs[i+1])
  performByDiv[i,2]<- length(want10mOGscale)
  
  want5m<- which(elev5m>=divs[i] & elev5m<= divs[i+1])
  elevIntervals5m[want5m]<- divs[i]
  print(paste0("# 5m cells: ", length(want5m)))
  performByDiv[i,3]<- length(want5m)
}

pctFloodedbyElev10m<- rep(NA,length(divs)-1)
pctFloodedbyElev5m<- rep(NA,length(divs)-1)

for(i in 1: 1:(length(divs)-1)){
  if(length(which(elevIntervals5m==divs[i]))>0){
    pctFloodedbyElev5m[i]<- mean(floodBin5mVals[which(elevIntervals5m==divs[i])])}
  if(length(which(elevIntervals5m==divs[i]))==0) pctFloodedbyElev5m[i]<-0
  if(length(which(elevIntervals10m==divs[i]))>0){
    pctFloodedbyElev10m[i]<- mean(floodBin10mat5mVals[which(elevIntervals10m==divs[i])])}
  if(length(which(elevIntervals10m==divs[i]))==0) pctFloodedbyElev10m[i]<-0
}

print(paste0("MAE: ", mean(abs(pctFloodedbyElev10m-pctFloodedbyElev5m))))
print(paste0("Mean difference: ", mean(pctFloodedbyElev10m-pctFloodedbyElev5m)))

#midpts<- divs[1:(length(divs)-1)]+len/2
midpts<- divs[1:(length(divs)-1)]+ (divs[2:length(divs)]-divs[1:(length(divs)-1)])/2

filename<- paste0("plots/",flood[f],"/ProbFloodbyElev10m.jpeg")
jpeg(file = filename,width = 700,height=700)
print(plot(midpts,pctFloodedbyElev10m,
           xlab="Approx. Elev",
           ylab="Probability of Being Flooded",
           main="Approx. Elev VS Probability of Flooding"))
print(points(midpts,pctFloodedbyElev5m,col="red"))
dev.off()

print(paste0("Difference > .05: ",midpts[which(abs(pctFloodedbyElev10m-pctFloodedbyElev5m)>.05)]))
print(paste0("Difference > .10: ",midpts[which(abs(pctFloodedbyElev10m-pctFloodedbyElev5m)>.1)]))
#across all resolutions, 
#given about the same cost of getting from the river to a point, 
#the probability of that point being flooded is about the same

LB<- divs[1:(length(divs)-1)]

save(pctFloodedbyElev10m,pctFloodedbyElev5m,len,divs,WetAndDryInds10m,
     minDryElev10m,maxWetElev10m,LB,midpts,
     file=paste0("data/",flood[f],"/pctFloodedbyElevAndLB_10mto5m.RData"))

ptFinal<-proc.time()-pt
time_pctFloodedbyElev<-ptFinal[3]
save(time_pctFloodedbyElev, file= paste0("data/",flood[f],"/time_pctFloodedbyElev_10mto5m.RData"))

