
rm(list=ls())

pt<-proc.time()

setwd("C:/Users/svr5482")

flow<- c("Q2559.8429","Q2503.2092","Q3681.19006")

for(f in 1:length(flow)){
  
  if(dir.exists(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/interpVarDnsclErrbyMeanAbsElevDiff"))==F){
    dir.create(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/interpVarDnsclErrbyMeanAbsElevDiff"))
  }
  
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/VD50to30.RData"))
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/VD50to10.RData"))
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/VD30to10.RData"))
  
  load("probabilisticDownscaling/data/simObs/meanabsDiffElev50to30.RData")
  load("probabilisticDownscaling/data/simObs/meanabsDiffElev50to10.RData")
  load("probabilisticDownscaling/data/simObs/meanabsDiffElev30to10.RData")
  
  y<- c(0,VD30to10,VD50to10,VD50to30)
  x<- c(0,meanabsDiffElev30to10,meanabsDiffElev50to10,meanabsDiffElev50to30)
  
  interpErrVar<- function(xNew){
    if(xNew<x[1]) return("Not possible")
    if(xNew>=x[1] & xNew <x[2]) return(linInterp(x[1],y[1],x[2],y[2],xNew))
    if(xNew>=x[2] & xNew <x[3]) return(linInterp(x[2],y[2],x[3],y[3],xNew))
    if(xNew>=x[3] & xNew <x[4]) return(linInterp(x[3],y[3],x[4],y[4],xNew))
    if(xNew>x[4]) return("out of bounds")
  }
  
  linInterp<- function(x0,y0,x1,y1,x){y0+(x-x0)*(y1-y0)/(x1-x0)}
  
  
  load("probabilisticDownscaling/data/simObs/meanabsDiffElev50to5.RData")
  load("probabilisticDownscaling/data/simObs/meanabsDiffElev30to5.RData")
  load("probabilisticDownscaling/data/simObs/meanabsDiffElev10to5.RData")
  
  xNew<- c(meanabsDiffElev10to5,meanabsDiffElev30to5,meanabsDiffElev50to5)
  yNew<- rep(NA,length(xNew))
  for(i in 1:length(xNew)) yNew[i]<- as.numeric(interpErrVar(xNew[i]))
  
  predVD10mto5m<- yNew[1]
  save(predVD10mto5m,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/interpVarDnsclErrbyMeanAbsElevDiff/predVD10mto5m.RData"))
  predVD30mto5m<- yNew[2]
  save(predVD30mto5m,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/interpVarDnsclErrbyMeanAbsElevDiff/predVD30mto5m.RData"))
  predVD50mto5m<- yNew[3]
  save(predVD50mto5m,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/interpVarDnsclErrbyMeanAbsElevDiff/predVD50mto5m.RData"))
  
  ptFinal<-proc.time()-pt
  time_predVarDnsclErr<-ptFinal[3]
  save(time_predVarDnsclErr, file= paste0("probabilisticDownscaling/data/",flow[f],"/simObs/interpVarDnsclErrbyMeanAbsElevDiff/time_interpPredVarDnsclErr.RData"))
  
  xNew<- seq(0.01,floor(max(x)*100)/100,by=.01)
  yNew<- rep(NA,length(xNew))
  for(i in 1:length(xNew)) yNew[i]<- as.numeric(interpErrVar(xNew[i]))
  
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/VD50to5.RData"))
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/VD30to5.RData"))
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/VD10to5.RData"))
  
  # Plot original points and interpolated values
  print(plot(x, y, type = "p", col = "blue", pch = 16, ylim = c(0, max(y)),
             main = "Linear Interpolation", xlab = "X", ylab = "Y"))
  print(points(xNew, yNew, col = "red"))
  
}



