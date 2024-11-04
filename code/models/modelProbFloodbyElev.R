#modelProbFloodByElev10m

#interpolate flooding probabilities between observed elevations


rm(list=ls())

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

if(dir.exists("data/modelProbFloodbyElev")==F){
  dir.create("data/modelProbFloodbyElev")
}

library(MASS)
library(Matrix)
library(fields)
library(mvtnorm)
library(spam)

#load percent of 30m cells flooded at each cost level
load("data/pctFloodedbyElevAndLB_10mto5m.RData")

pt<-proc.time()

LB<- divs[1:(length(divs)-1)]

nElev=length(LB)+2

################################################################################

elevs<- c(floor(LB[1]*100)/100,LB+len/2,ceiling(divs[length(divs)]*100)/100)
elev.c<- elevs/max(elevs)

allPctFloodedbyElev10m<- c(1,pctFloodedbyElev10m,0)
y.c10<- allPctFloodedbyElev10m- mean(allPctFloodedbyElev10m)

##################################################
#likelihood function for the covariance parameter#
##################################################

SS=elev.c
AA = matrix(SS,length(SS),length(SS))
BB = t(AA)
C1 = abs(AA-BB)

#fix the value of nugget
zeta=1e-5
#I will fix the nugget to avoid computational instability.

#Now use all the observations to get the parameters

log.posterior1=function(kappa,phi)
{
  Y.red=y.c10
  cov.red=zeta*diag(1,nElev)+kappa*exp(-C1/phi^2)
  log.l=-0.5*determinant(cov.red,logarithm=TRUE)$modulus[1]-0.5*t(Y.red)%*%solve(cov.red,Y.red)
  if(prod(phi>0)*prod(kappa>0)) return(log.l) else return(-Inf)
}

#wrapper code
wrap.log.posterior1=function(x){ -log.posterior1(x[1],x[2])}

#matrix to store the correlation parameters

par.est=nlminb(c(1,1),wrap.log.posterior1)$par
kappa.10=par.est[1]
phi.10=par.est[2]


save(kappa.10,phi.10,
     file="data/modelProbFloodbyElev/parsGP.10mto5m.FN1e-5.RData")

ptFinal<-proc.time()-pt
time_modelProbFloodbyElev<-ptFinal[3]
save(time_modelProbFloodbyElev, file= "data/time_modelProbFloodbyElev_10mto5m.RData")
