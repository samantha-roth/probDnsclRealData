#interpolate costs between the observed

rm(list=ls())

setwd("C:/Users/svr5482")

library(MASS)
library(Matrix)
library(fields)
library(mvtnorm)
library(spam)
#library(mcmc)

flow<- c("Q2559.8429","Q2503.2092","Q3681.19006")

for(f in 1:length(flow)){
  
  if(dir.exists(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/modelProbFloodbyCost"))==F){
    dir.create(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/modelProbFloodbyCost"))}
  
  ################################################################################
  ################################################################################
  #10m

  setwd("C:/Users/svr5482")
  
  #load percent of 10m cells flooded at each cost level
  load(paste0("probabilisticDownscaling/data/",flow[f],"/pctFloodedbyCostAndLB_10mto5m.RData"))
  
  pt<-proc.time()
  
  nCost=length(LB)
  
  
  ################################################################################
  
  cost.c<- (LB+250)/max(LB)
  
  y.c10<- pctFloodedbyCost10m- mean(pctFloodedbyCost10m)
  
  ##################################################
  #likelihood function for the covariance parameter#
  ##################################################
  
  SS=cost.c
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
    cov.red=zeta*diag(1,nCost)+kappa*exp(-C1/phi^2)
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
       file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/modelProbFloodbyCost/parsGP.10mto5m.FN1e-5.RData"))
  
  ptFinal<-proc.time()-pt
  time_modelProbFloodbyCost<-ptFinal[3]
  save(time_modelProbFloodbyCost, file= paste0("probabilisticDownscaling/data/",flow[f],"time_modelProbFloodbyCost_10mto5m.RData"))
  
  ################################################################################
  ################################################################################
  #30m

  setwd("C:/Users/svr5482")
  
  #load percent of 30m cells flooded at each cost level
  load(paste0("probabilisticDownscaling/data/",flow[f],"/pctFloodedbyCostAndLB_30mto5m.RData"))
  
  pt<-proc.time()
  
  nCost=length(LB)
  
  
  ################################################################################
  
  cost.c<- (LB+250)/max(LB)
  
  y.c30<- pctFloodedbyCost30m- mean(pctFloodedbyCost30m)
  
  ##################################################
  #likelihood function for the covariance parameter#
  ##################################################
  
  SS=cost.c
  AA = matrix(SS,length(SS),length(SS))
  BB = t(AA)
  C1 = abs(AA-BB)
  
  #fix the value of nugget
  zeta=1e-5
  #I will fix the nugget to avoid computational instability.
  
  #Now use all the observations to get the parameters
  
  log.posterior1=function(kappa,phi)
  {
    Y.red=y.c30
    cov.red=zeta*diag(1,nCost)+kappa*exp(-C1/phi^2)
    log.l=-0.5*determinant(cov.red,logarithm=TRUE)$modulus[1]-0.5*t(Y.red)%*%solve(cov.red,Y.red)
    if(prod(phi>0)*prod(kappa>0)) return(log.l) else return(-Inf)
  }
  
  #wrapper code
  wrap.log.posterior1=function(x){ -log.posterior1(x[1],x[2])}
  
  #matrix to store the correlation parameters
  
  par.est=nlminb(c(1,1),wrap.log.posterior1)$par
  kappa.30=par.est[1]
  phi.30=par.est[2]
  
  
  save(kappa.30,phi.30,
       file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/modelProbFloodbyCost/parsGP.30mto5m.FN1e-5.RData"))
  
  ptFinal<-proc.time()-pt
  time_modelProbFloodbyCost<-ptFinal[3]
  save(time_modelProbFloodbyCost, file= paste0("probabilisticDownscaling/data/",flow[f],"time_modelProbFloodbyCost_30mto5m.RData"))
  
  ################################################################################
  ################################################################################
  #50m

  setwd("C:/Users/svr5482")
  
  #load percent of 50m cells flooded at each cost level
  load(paste0("probabilisticDownscaling/data/",flow[f],"/pctFloodedbyCostAndLB_50mto5m.RData"))
  
  pt<-proc.time()
  
  nCost=length(LB)
  
  
  ################################################################################
  
  cost.c<- (LB+250)/max(LB)
  
  y.c50<- pctFloodedbyCost50m- mean(pctFloodedbyCost50m)
  
  ##################################################
  #likelihood function for the covariance parameter#
  ##################################################
  
  SS=cost.c
  AA = matrix(SS,length(SS),length(SS))
  BB = t(AA)
  C1 = abs(AA-BB)
  
  #fix the value of nugget
  zeta=1e-5
  #I will fix the nugget to avoid computational instability.
  
  #Now use all the observations to get the parameters
  
  log.posterior1=function(kappa,phi)
  {
    Y.red=y.c50
    cov.red=zeta*diag(1,nCost)+kappa*exp(-C1/phi^2)
    log.l=-0.5*determinant(cov.red,logarithm=TRUE)$modulus[1]-0.5*t(Y.red)%*%solve(cov.red,Y.red)
    if(prod(phi>0)*prod(kappa>0)) return(log.l) else return(-Inf)
  }
  
  #wrapper code
  wrap.log.posterior1=function(x){ -log.posterior1(x[1],x[2])}
  
  #matrix to store the correlation parameters
  
  par.est=nlminb(c(1,1),wrap.log.posterior1)$par
  kappa.50=par.est[1]
  phi.50=par.est[2]
  
  
  save(kappa.50,phi.50,
       file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/modelProbFloodbyCost/parsGP.50mto5m.FN1e-5.RData"))
  
  ptFinal<-proc.time()-pt
  time_modelProbFloodbyCost<-ptFinal[3]
  save(time_modelProbFloodbyCost, file= paste0("probabilisticDownscaling/data/",flow[f],"time_modelProbFloodbyCost_50mto5m.RData"))
  
  ################################################################################
  
  #load("probabilisticDownscaling/data/simObs/modelProbFloodbyCost/parsGP.10mto5m.FN1e-5.RData")
  #load("probabilisticDownscaling/data/simObs/modelProbFloodbyCost/parsGP.30mto5m.FN1e-5.RData")
  #load("probabilisticDownscaling/data/simObs/modelProbFloodbyCost/parsGP.50mto5m.FN1e-5.RData")
  
  ################################################################################
  
}
