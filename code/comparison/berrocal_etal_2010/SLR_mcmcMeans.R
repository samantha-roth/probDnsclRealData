
rm(list=ls())

# dir<- commandArgs(trailingOnly=TRUE)
# setwd(dir)
setwd("/Volumes/RothS/probDnsclRealData")
# setwd("/dartfs/rc/lab/R/RothS/probDnsclRealData")

load("data/mcmc.output1")

beta0_mean<- mean(c(mcmc.output1$chain1[,"beta0"],
                    mcmc.output1$chain2[,"beta0"],
                    mcmc.output1$chain3[,"beta0"],
                    mcmc.output1$chain4[,"beta0"]))

beta1_mean<- mean(c(mcmc.output1$chain1[,"beta1"],
                    mcmc.output1$chain2[,"beta1"],
                    mcmc.output1$chain3[,"beta1"],
                    mcmc.output1$chain4[,"beta1"]))

sigma2_mean<- mean(c(mcmc.output1$chain1[,"sigma2"],
                     mcmc.output1$chain2[,"sigma2"],
                     mcmc.output1$chain3[,"sigma2"],
                     mcmc.output1$chain4[,"sigma2"]))

mcmc.means1<- c(beta0_mean,
                beta1_mean,
                sigma2_mean)
names(mcmc.means1)<- c("beta0",
                       "beta1",
                       "sigma2")

save(mcmc.means1,file="data/mcmc.means1")