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

#load the 10m res water surface heights
load("data/wsh.10m_HWMlocs")

#use to inform priors
SLR_fit<- lm(obs~wsh.10m) #to inform priors for beta0, beta1, and sigma2
beta0_est<- as.numeric(SLR_fit$coefficients[1])
beta1_est<- as.numeric(SLR_fit$coefficients[2])
sigma2_est<- mean(SLR_fit$residuals^2) 


################################################################################
#variations of models with different hyperpriors

#wide, uninformed by SLR
model1 <- nimbleCode({ 
  # likelihood
  for(i in 1:N) {
    y[i] ~ dnorm(mu[i], var = sigma2)
    mu[i] <- beta0 + beta1 * x[i]
  }
  # priors
  beta0 ~ dnorm(0, sd = 3)
  beta1 ~ dnorm(0, sd = 3)
  sigma2 ~ dinvgamma(shape = 2, scale = 1)
})

#generate random initial values for all parameters
initial.values.function.model1 <- function() {
  return(list(beta0 = rnorm(1, mean=0, sd=3),
              beta1 = rnorm(1, mean=0, sd=3), 
              sigma2 = rinvgamma(1, shape= 1, scale=1)))
}

#narrow, informed by SLR
model2 <- nimbleCode({ 
  # likelihood
  for(i in 1:N) {
    y[i] ~ dnorm(mu[i], var = sigma2)
    mu[i] <- beta0 + beta1 * x[i]
  }
  # priors
  beta0 ~ dnorm(beta0_est, sd = 1)
  beta1 ~ dnorm(beta1_est, sd = 1)
  sigma2 ~ dinvgamma(shape = 24, scale = 1)
})

#generate random initial values for all parameters
initial.values.function.model2 <- function() {
  return(list(beta0 = rnorm(1, mean=beta0_est, sd=1),
              beta1 = rnorm(1, mean=beta1_est, sd=1), 
              sigma2 = rinvgamma(1, shape= 24, scale=1)))
}

#wide, semi-informed by SLR
model3 <- nimbleCode({ 
  # likelihood
  for(i in 1:N) {
    y[i] ~ dnorm(mu[i], var = sigma2)
    mu[i] <- beta0 + beta1 * x[i]
  }
  # priors
  beta0 ~ dnorm(beta0_est, sd = 3)
  beta1 ~ dnorm(beta1_est, sd = 3)
  sigma2 ~ dinvgamma(shape = 2, scale = 1)
})

#generate random initial values for all parameters
initial.values.function.model3 <- function() {
  return(list(beta0 = rnorm(1, mean=beta0_est, sd=3),
              beta1 = rnorm(1, mean=beta1_est, sd=3), 
              sigma2 = rinvgamma(1, shape= 2, scale=1)))
}
################################################################################

#common to all models

# Data
my.data <- list(
  y = obs,
  x = wsh.10m
)

parameters.to.save <- c("beta0", "beta1", "sigma2")

n.iter <- 10000
n.burnin <- 1000
n.chains <- 4
################################################################################

#model1
my.constants <- list(N = 5)

# MCMC settings
set.seed(666)

# Fit model
mcmc.output1 <- nimbleMCMC(code = model1,
                          data = my.data,
                          constants = my.constants,
                          inits = initial.values.function.model1,
                          monitors = parameters.to.save,
                          niter = n.iter,
                          nburnin = n.burnin,
                          nchains = n.chains)

# Examine results
MCMCsummary(object = mcmc.output1, round = 2)

MCMCtrace(object = mcmc.output1,
          pdf = FALSE,
          ind = TRUE,
          params = "sigma2")

MCMCtrace(object = mcmc.output1,
          pdf = FALSE,
          ind = TRUE,
          params = "beta0")

MCMCtrace(object = mcmc.output1,
          pdf = FALSE,
          ind = TRUE,
          params = "beta1")

save(mcmc.output1,file="data/mcmc.output1")

################################################################################

#model2
my.constants <- list(N = 5, beta0_est= beta0_est, beta1_est= beta1_est)

# MCMC settings
set.seed(666)

# Fit model
mcmc.output2 <- nimbleMCMC(code = model2,
                           data = my.data,
                           constants = my.constants,
                           inits = initial.values.function.model2,
                           monitors = parameters.to.save,
                           niter = n.iter,
                           nburnin = n.burnin,
                           nchains = n.chains)

# Examine results
MCMCsummary(object = mcmc.output2, round = 2)

MCMCtrace(object = mcmc.output2,
          pdf = FALSE,
          ind = TRUE,
          params = "sigma2")

MCMCtrace(object = mcmc.output2,
          pdf = FALSE,
          ind = TRUE,
          params = "beta0")

MCMCtrace(object = mcmc.output2,
          pdf = FALSE,
          ind = TRUE,
          params = "beta1")

save(mcmc.output2,file="data/mcmc.output2")

################################################################################

#model3
my.constants <- list(N = 5, beta0_est= beta0_est, beta1_est= beta1_est)

# MCMC settings
set.seed(666)

# Fit model
mcmc.output3 <- nimbleMCMC(code = model3,
                           data = my.data,
                           constants = my.constants,
                           inits = initial.values.function.model3,
                           monitors = parameters.to.save,
                           niter = n.iter,
                           nburnin = n.burnin,
                           nchains = n.chains)

# Examine results
MCMCsummary(object = mcmc.output3, round = 2)

MCMCtrace(object = mcmc.output3,
          pdf = FALSE,
          ind = TRUE,
          params = "sigma2")

MCMCtrace(object = mcmc.output3,
          pdf = FALSE,
          ind = TRUE,
          params = "beta0")

MCMCtrace(object = mcmc.output3,
          pdf = FALSE,
          ind = TRUE,
          params = "beta1")

save(mcmc.output3,file="data/mcmc.output3")

################################################################################
#additional ways to look at posteriors
# Extract posterior means
mean(mcmc.output1$chain1[,'beta0'])
mean(mcmc.output1$chain1[,'beta1'])
mean(mcmc.output1$chain1[,'sigma'])

# 95% credible intervals
quantile(mcmc.output1$chain1[,'sigma'], probs = c(0.025, 0.975))

quantile(mcmc.output1$chain1[,'beta1'], probs = c(0.025, 0.975))



