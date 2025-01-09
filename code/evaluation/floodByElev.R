rm(list=ls())

library(terra)
library(DescTools)
library(ggplot2)

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

pt<-proc.time()

#get coordinates of interest
run5m<- rast("data/Outputs5m/Run_1.asc")
run10m<- rast("data/Outputs10m/Run_1.asc")
load("data/coords.5m.RData")
load("data/coords.10m.RData")

#determine 5m locations that are within the bounds of the 10m raster
x.min.10m<- min(coords.10m[,"x"]); x.max.10m<- max(coords.10m[,"x"])
y.min.10m<- min(coords.10m[,"y"]); y.max.10m<- max(coords.10m[,"y"])

x.min.5m<- min(coords.5m[,"x"]); x.max.5m<- max(coords.5m[,"x"])
y.min.5m<- min(coords.5m[,"y"]); y.max.5m<- max(coords.5m[,"y"])

xInBds<- intersect(which(coords.5m[,"x"]>x.min.10m),which(coords.5m[,"x"]<x.max.10m))
yInBds<- intersect(which(coords.5m[,"y"]>y.min.10m),which(coords.5m[,"y"]<y.max.10m))
goodInds5m<- intersect(xInBds,yInBds)

vals5m<- c(as.matrix(extract(run5m,coords.5m[goodInds5m,])))
vals10mat5m<- c(as.matrix(extract(run10m,coords.5m[goodInds5m,])))

floodBin10mat5mVals<- ifelse(vals10mat5m>0,1,0)
floodBin5mVals<- ifelse(vals5m>0,1,0)

#load files giving elevation

dem5m<- rast("data/norristown_5m.asc")
dem10m<- rast("data/norristown_10m_new.asc")

elev5m<- c(as.matrix(extract(dem5m,coords.5m[goodInds5m,])))
elev10mat5m<- c(as.matrix(extract(dem10m,coords.5m[goodInds5m,])))
elev10m<- c(as.matrix(extract(dem10m,coords.10m)))
################################################################################

#now see if there's a relationship between elevation
#and whether or not the location is flooded

plot(floodBin10mat5mVals,elev10mat5m)
plot(floodBin5mVals,elev5m)

maxWetElev10m<- max(elev10mat5m[which(floodBin10mat5mVals==1)])
maxWetElev5m<- max(elev5m[which(floodBin5mVals==1)])
#whenever elev is >25.14905 at 10m the location is not flooded
#whenever elev is >25.38679 at 5m the location is not flooded


minDryElev10m<- min(elev10mat5m[which(floodBin10mat5mVals==0)])
minDryElev5m<- min(elev5m[which(floodBin5mVals==0)])
#whenever elev is <21.92426 at 10m the location is flooded
#whenever elev is <22.03399 at 5m the location is flooded

################################################################################
WetAndDryInds10m<- which(elev10mat5m>minDryElev10m & elev10mat5m<maxWetElev10m)
#hist(elev10mat5m[WetAndDryInds10m])
max(elev10mat5m[WetAndDryInds10m])
min(elev10mat5m[WetAndDryInds10m])
len<- (maxWetElev10m-minDryElev10m)/8

divs<- seq(minDryElev10m,maxWetElev10m,by=len)
#divs<- c(divs[1],divs[3:5],divs[5]+len/2,divs[6],divs[6]+len/2,divs[7:length(divs)])
#divs<- c(divs[1],divs[3:length(divs)])

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

#plot(abs(performByDiv[,2]-performByDiv[,3]),abs(pctFloodedbyCost10m-pctFloodedbyCost5m))
mean(abs(pctFloodedbyElev10m-pctFloodedbyElev5m))
mean(pctFloodedbyElev10m-pctFloodedbyElev5m)

midpts<- divs[1:(length(divs)-1)]+len/2

pctFloodbyElev.df<- data.frame("Elevation"= c(midpts,midpts),
                               "pctFlooded"= c(pctFloodedbyElev10m,pctFloodedbyElev5m),
                               "Resolution"= c(rep("10m",length(midpts)),rep("5m",length(midpts))))

filename<- paste0("plots/elevVSfloodprob.jpeg")
jpeg(file = filename,width = 600,height=500)
print(ggplot(data = pctFloodbyElev.df, aes(x = Elevation, y = pctFlooded, color = Resolution)) +
  geom_point() +
  ggtitle("Elevation VS flooding probability") +
  ylab("Flooding probability")+ xlab("Elevation (m)")+
  theme(plot.title = element_text(size=24), 
        axis.title = element_text(size=24),
        axis.text = element_text(size = 20),
        legend.text= element_text(size=24),
        legend.title= element_text(size=24)))
dev.off()


plot(midpts,pctFloodedbyElev10m,
     xlab="Approx. Elev",
     ylab="Probability of Being Flooded",
     main="Approx. Elev VS Probability of Flooding")
points(midpts,pctFloodedbyElev5m,col="red")


midpts

filename<- paste0("plots/elev_vs_pctflooded.jpeg")
jpeg(file = filename,width = 600,height=500)
print(ggplot(data=predsVSobs,aes(x=preds5m,y=obs))+
        geom_point(color=,size=5)+
        geom_abline(intercept=0,slope=1,lwd = 2) +
        ylab("Observed Flood Height (m)")+ xlab("5m Resolution Predicted Flood Height (m)")+
        theme_bw() + 
        theme(plot.title = element_text(size=24), 
              axis.title = element_text(size=24),
              axis.text = element_text(size = 20),
              legend.text= element_text(size=24),
              legend.title= element_text(size=24)))
dev.off()


midpts[which(abs(pctFloodedbyElev10m-pctFloodedbyElev5m)>.05)]
midpts[which(abs(pctFloodedbyElev10m-pctFloodedbyElev5m)>.1)]
#across all resolutions, 
#given about the same Elev, 
#the probability of that point being flooded is about the same

LB<- divs[1:(length(divs)-1)]
#save(pctFloodedbyElev10m,pctFloodedbyElev10m,pctFloodedbyElev5m,LB,
#     file="data/pctFloodedbyElevAndLB.RData")

save(pctFloodedbyElev10m,pctFloodedbyElev5m,len,divs,WetAndDryInds10m,
     minDryElev10m,maxWetElev10m,
     file="data/pctFloodedbyElevAndLB_10mto5m.RData")

ptFinal<-proc.time()-pt
time_pctFloodedbyElev<-ptFinal[3]
save(time_pctFloodedbyElev, file= "data/time_pctFloodedbyElev_10mto5m.RData")