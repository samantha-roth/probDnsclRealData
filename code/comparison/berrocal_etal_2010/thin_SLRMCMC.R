rm(list=ls())

library(terra)

pt<- proc.time()

#load the mcmc samples from fitting the SLR model
load("data/mcmc.output1")

beta0_samples<- c(mcmc.output1$chain1[,"beta0"],
                  mcmc.output1$chain2[,"beta0"],
                  mcmc.output1$chain3[,"beta0"],
                  mcmc.output1$chain4[,"beta0"])

beta1_samples<- c(mcmc.output1$chain1[,"beta1"],
                  mcmc.output1$chain2[,"beta1"],
                  mcmc.output1$chain3[,"beta1"],
                  mcmc.output1$chain4[,"beta1"])

sigma2_samples<- c(mcmc.output1$chain1[,"sigma2"],
                   mcmc.output1$chain2[,"sigma2"],
                   mcmc.output1$chain3[,"sigma2"],
                   mcmc.output1$chain4[,"sigma2"])

thin_inds<- 36*(1:1000)

beta0_thin<- beta0_samples[thin_inds]
beta1_thin<- beta1_samples[thin_inds]
sigma2_thin<- sigma2_samples[thin_inds]

#compare thinned samples to originals
quantile(beta0_samples, probs=c(0.025,0.1,0.5,0.9,0.975))
quantile(beta0_thin, probs=c(0.025,0.1,0.5,0.9,0.975))
plot(density(beta0_samples)); lines(density(beta0_thin),col="red")

quantile(beta1_samples, probs=c(0.025,0.1,0.5,0.9,0.975))
quantile(beta1_thin, probs=c(0.025,0.1,0.5,0.9,0.975))
plot(density(beta1_samples)); lines(density(beta1_thin),col="red")

quantile(sigma2_samples, probs=c(0.025,0.1,0.5,0.9,0.975))
quantile(sigma2_thin, probs=c(0.025,0.1,0.5,0.9,0.975))
plot(density(sigma2_samples)); lines(density(sigma2_thin),col="red")

#all look close enough to being the same


save(beta0_thin,beta1_thin,sigma2_thin,file="data/mcmc.output1_thinned")

ptFinal<-proc.time()-pt
time_thinChain_SLRMCMC<-ptFinal[3]
save(time_thinChain_SLRMCMC, file= "data/time_thinChain_SLRMCMC.RData")