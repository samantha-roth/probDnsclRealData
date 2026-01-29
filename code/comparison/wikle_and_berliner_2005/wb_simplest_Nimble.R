#nonspatial wikle and berliner 2005

rm(list=ls())
graphics.off()

library(terra)
library(nimble)
library(MCMCvis)

setwd("/Users/f007f8t/Documents/GitHub/probDnsclRealData")

#load the high water marks
load("data/HWMsdf.RData")
HWMlocs<- as.matrix(HWMs.df[,c("x","y")]); HWMlocs<- HWMlocs[1:5,]
obs<- HWMs.df$height[1:5]

#load observation variances
load("data/obserror_est.RData")
z_s2<- z_s2_est95[1:5]; rm(z_s2_est95)

#load the 10m res water surface heights
load("data/wsh.10m_HWMlocs")

################################################################################
#######variations of Wikle and Berliner's model with different assumptions######
################################################################################

#first, we'll do the simplest version where we just fit a model for the HWMs

#wide, uninformed by SLR
model_HWMs <- nimbleCode({ 
  # likelihood
  for(i in 1:N_a) {
    z_a[i] ~ dnorm(y_b[i], var = tau2_a[i])
    y_b[i] ~ dnorm(0, sd = 5) #the five 5m cells that correspond to HWM locations
  }
  
})

#generate random initial values for all parameters
initial.values.function.model_HWMs <- function() {
  return(list(y_b = rnorm(5, mean=0, sd=5)))
}

my.constants_HWMs <- list(N_a = 5,
                     tau2_a = z_s2)

my.data_HWMs <- list(
  z_a = obs
)

parameters.to.save_HWMs <- c("y_b")

n.iter <- 2000
n.burnin <- 500
n.chains <- 4

# MCMC settings
set.seed(666)

# Fit model
mcmc.output1 <- nimbleMCMC(code = model_HWMs,
                           data = my.data_HWMs,
                           constants = my.constants_HWMs,
                           inits = initial.values.function.model_HWMs,
                           monitors = parameters.to.save_HWMs,
                           niter = n.iter,
                           nburnin = n.burnin,
                           nchains = n.chains)

MCMCsummary(object = mcmc.output1, round = 2)

################################################################################

#next, we'll make the model include the low-resolution flood heights that
#contain high water marks

#wide, uninformed by SLR
model_HWMs2 <- nimbleCode({ 
  # likelihood
  for(i in 1:N_a) {
    z_a[i] ~ dnorm(y_b[i], var = tau2_a[i])
    z_c[i] ~ dnorm(y_b[i], var = tau2_c)
    y_b[i] ~ dnorm(0, sd = 5) #the five 5m cells that correspond to HWM locations
  }
  tau2_c ~ dinvgamma(shape = 2, scale = 1)
})

#generate random initial values for all parameters
initial.values.function.model_HWMs2 <- function() {
  return(list(y_b = rnorm(5, mean=0, sd=5), 
              tau2_c = rinvgamma(1, shape= 2, scale=1)))
}

my.constants_HWMs2 <- list(N_a = 5,
                          tau2_a = z_s2)

my.data_HWMs2 <- list(
  z_a = obs, 
  z_c = wsh.10m
)

parameters.to.save_HWMs2 <- c("y_b","tau2_c")

n.iter <- 2000
n.burnin <- 500
n.chains <- 4

# MCMC settings
set.seed(666)

# Fit model
mcmc.output2 <- nimbleMCMC(code = model_HWMs2,
                           data = my.data_HWMs2,
                           constants = my.constants_HWMs2,
                           inits = initial.values.function.model_HWMs2,
                           monitors = parameters.to.save_HWMs2,
                           niter = n.iter,
                           nburnin = n.burnin,
                           nchains = n.chains)

MCMCsummary(object = mcmc.output2, round = 2)

MCMCtrace(object = mcmc.output2,
          pdf = FALSE,
          ind = TRUE,
          params = "tau2_c")

MCMCtrace(object = mcmc.output2,
          pdf = FALSE,
          ind = TRUE,
          params = "y_b")


