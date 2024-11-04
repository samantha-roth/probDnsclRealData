#get the probability of flooding at the locations of interest based on the model
#for probability of flooding given elevation

rm(list=ls())

library(terra)

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

################################################################################
################################################################################
#10m to 5m

pt<-proc.time()

run5m<- rast("data/Outputs5m/Run_1.asc")
load("data/coords.5m.RData")
inds.5m<- 1:ncell(run5m)

#load files giving cost of getting from river to point considering elevation
dem5m<- rast("data/norristown_5m.asc")

#load cells of interest outside the low res flooded cells
load("data/destInds10mat5m.RData")
coords.5mDest<- coords.5m[destInds10m,]

#get 5m costs at the destination coords
elevAtDestCoords5m<- c(as.matrix(extract(dem5m,coords.5mDest)))

################################################################################
#Now use the cost to flooding probability models to 
#determine the probability of being flooded 

#load parameters from the Gaussian processes
load("data/modelProbFloodbyElev/parsGP.10mto5m.FN1e-5.RData")

#load training data (costs and flooding probabilities)
load("data/pctFloodedbyElevAndLB_10mto5m.RData")
LB<- divs[1:(length(divs)-1)]
nElev=length(LB)+2
################################################################################
#center the % flooded

elevs<- c(floor(LB[1]*100)/100,LB+len/2,ceiling(divs[length(divs)]*100)/100)
elev.c<- elevs/max(elevs)

allPctFloodedbyElev10m<- c(1,pctFloodedbyElev10m,0)
y.c10<- allPctFloodedbyElev10m- mean(allPctFloodedbyElev10m)

#make predictions at 5m costs of destination cells
elevToPred<- elevAtDestCoords5m
nNewCost= length(elevToPred)
elevToPred.c<- elevToPred/max(elevs)

################################################################################
#compute distance matrix with new costs to predict at
SS=c(elevToPred.c,elev.c)
AA = matrix(SS,length(SS),length(SS))
BB = t(AA)
C1 = abs(AA-BB)
################################################################################
nTrain= nElev
nTest= length(elevToPred)
nAll<- nTrain+ nTest
testInds<- 1:nTest

################################################################################
#make predictions at the new cost locations based on the function fit 
#using 10m flooding probabilities
zeta=1e-5

cov.red=zeta*diag(1,nAll)+kappa.10*exp(-C1/phi.10^2) #compute the covariance function
Sigma22=cov.red[-testInds,-testInds] #Sigma_22 matrix
Sigma12=matrix(cov.red[testInds,-testInds],nTest,nTrain) #Sigma matrix
predicted=Sigma12%*%solve(Sigma22)%*%y.c10 #predicted values from the emulator

predProbFlood<- predicted + mean(allPctFloodedbyElev10m)
predProbFlood[which(elevToPred>divs[length(divs)])]<-0
predProbFlood[which(elevToPred<divs[1])]<-1

save(predProbFlood,
     file="data/modelProbFloodbyElev/predProbFloodatDest_5mElev10mModel.RData")

ptFinal<-proc.time()-pt
time_predProbFlood<-ptFinal[3]
save(time_predProbFlood, file= "data/modelProbFloodbyElev/time_predProbFlood10mto5m.RData")


filename<- "plots/ProbFloodbyElevFuncAtDestPts10mto5m.jpeg"
jpeg(file = filename,width = 700,height=700)
plot(elevToPred,predProbFlood,
     main="Probability Flooded = f(Elevation)",
     xlab="Elevation",
     ylab="Interpolated P(Flooded) From 10m Flooded Locs")
points(elevs,allPctFloodedbyElev10m,col="red",pch=16)
dev.off()
