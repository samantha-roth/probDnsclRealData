#compute bounds of 95% prediction interval 
#for high resultion flood heights within low resolution wet cells

rm(list=ls())

#load the bilinearly interpolated 10m projections at the HWM locations
dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

pt<-proc.time()

load("data/varResHWM10mto5m.RData")
n_obs=5
df=n_obs-1

save(varResHWM10m,file="data/varResHWM10mto5m.RData")

#use that (n-1)*varResHWM10m/(sigma^2) ~ Chi-Square(n-1)

#varResHWM10m ~ sigma^2* Chi-Square(n-1) / (n-1)
#estimate sigma^2 with varResHWM10m

set.seed(22)
chisq_samples<- rchisq(n = 5, df = df)
var_samples<- varResHWM10m*chisq_samples/df
save(var_samples,file="data/var_samples")
