rm(list=ls())
graphics.off()

library(nimble)
library(MCMCvis)

setwd("/Users/f007f8t/Documents/GitHub/probDnsclRealData")

#load the high water marks
load("data/HWMsdf.RData")
HWMlocs<- as.matrix(HWMs.df[,c("x","y")]); HWMlocs<- HWMlocs[1:5,]
obs<- HWMs.df$height[1:5]

#load the 10m res water surface heights
load("data/wsh.10m_HWMlocs")

#preliminary model to inform priors
SLR_fit<- lm(obs~wsh.10m) #to inform priors for beta0 and beta1
mse<- mean(SLR_fit$residuals^2) #to inform tau2

# Data
my.data <- list(
  y = obs,
  x = wsh.10m
)

# Locations
# locations <- matrix(c(210, 325,
#                       92, 6,
#                       202, 292,
#                       0, 2,
#                       192, 0), nrow = 5, ncol = 2, byrow = TRUE)

locations<- HWMlocs

# Calculate distance matrix
dist_matrix <- as.matrix(dist(locations))

# Add the covariance matrix computation to the model
model <- nimbleCode({
  
  #y is now the sum of two gaussian processes
  for(i in 1:N) {
    y[i] <- w[i] + z[i]
  }
  
  #main gaussian process
  z[1:N] ~ dmnorm(mu[1:N], cov = Sigma[1:N, 1:N])
  
  #spatial random effect
  w[1:N] ~ dmnorm(zeros[1:N], cov = Sigma_spatial[1:N, 1:N])
  
  #define mu
  for(i in 1:N) {
    mu[i] <- beta0 + beta1 * x[i]
  }
  
  # Build spatial covariance matrix with exponential covariance function
  for(i in 1:N) {
    for(j in 1:N) {
      Sigma_spatial[i, j] <- sigma2 * exp(-phi * dist[i, j])
    }
  }
  
  # Covariance for y (measurement error only)
  for(i in 1:N) {
    for(j in 1:N) {
      Sigma[i, j] <- tau2 * isdiag[i, j]
    }
  }
  
  # priors
  beta0 ~ dnorm(0.75, sd = 2)
  beta1 ~ dnorm(0.72, sd = 2)
  tau2 ~ dinvgamma(shape = 1, rate = 1)
  phi ~ dunif(0.01, 5)  # spatial range parameter
  sigma2 ~ dinvgamma(shape = 1, rate = 1)
})

# Add distance matrix to constants
my.constants <- list(N = 5, dist = dist_matrix)

parameters.to.save <- c("beta0", "beta1", "tau2", "phi", "sigma2")

# Initial values

initial.values <- list(
  list(beta0 = 0.75, beta1 = 0.72, sigma2 = 0.5, tau2 = 0.04, phi = 0.1),
  list(beta0 = 1, beta1 = 0.5, sigma2 = 0.8, tau2 = 0.02, phi = 0.5),
  list(beta0 = 0.5, beta1 = 0.8, sigma2 = 1.0, tau2 = 0.1, phi = 1.0)
)

# MCMC settings
set.seed(666)
n.iter <- 10000
n.burnin <- 1000
n.chains <- 3

# Fit model
mcmc.output <- nimbleMCMC(code = model,
                          data = my.data,
                          constants = my.constants,
                          inits = initial.values,
                          monitors = parameters.to.save,
                          niter = n.iter,
                          nburnin = n.burnin,
                          nchains = n.chains)

# Examine results
MCMCsummary(object = mcmc.output, round = 2)

MCMCtrace(object = mcmc.output,
          pdf = FALSE,
          ind = TRUE,
          params = "beta0")

MCMCtrace(object = mcmc.output,
          pdf = FALSE,
          ind = TRUE,
          params = "beta1")

MCMCtrace(object = mcmc.output,
          pdf = FALSE,
          ind = TRUE,
          params = "tau2")

MCMCtrace(object = mcmc.output,
          pdf = FALSE,
          ind = TRUE,
          params = "phi")

MCMCtrace(object = mcmc.output,
          pdf = FALSE,
          ind = TRUE,
          params = "sigma2")

#we're inferring basically nothing about phi
#also sigma2 and tau2 are unstable

# Extract posterior means
mean(mcmc.output$chain1[,'beta0'])
mean(mcmc.output$chain1[,'beta1'])
mean(mcmc.output$chain1[,'tau2'])
mean(mcmc.output$chain1[,'sigma2'])
mean(mcmc.output$chain1[,'phi'])

#extract posterior medians
median(mcmc.output$chain1[,'beta0'])
median(mcmc.output$chain1[,'beta1'])
median(mcmc.output$chain1[,'tau2'])
median(mcmc.output$chain1[,'sigma2'])
median(mcmc.output$chain1[,'phi'])

# 95% credible intervals
quantile(mcmc.output$chain1[,'phi'], probs = c(0.025, 0.975))