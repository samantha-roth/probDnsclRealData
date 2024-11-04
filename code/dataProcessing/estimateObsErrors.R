
#decide on priors for observational errors

rm(list=ls())

load("/storage/work/svr5482/Reification/Philly/data/Manayunk/HWMsdf.RData")
load("/storage/work/svr5482/Reification/Philly/data/Manayunk/hwmQualities.RData")

hwmQualities

hwmQualitiesM<- c(0.03048*2,0.03048,0.03048,0.03048*5,0.03048*4,0.03048,0.03048*2)

z_s2_est95= (hwmQualitiesM/1.96)^2

save(z_s2_est95,file="/storage/work/svr5482/Reification/Philly/data/Manayunk/obserror_est.RData")
