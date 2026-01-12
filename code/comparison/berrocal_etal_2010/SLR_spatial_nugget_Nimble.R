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
  # likelihood with spatial correlation
  y[1:N] ~ dmnorm(mu[1:N], cov = Sigma[1:N, 1:N])
  
  for(i in 1:N) {
    mu[i] <- beta0 + beta1 * x[i]
  }
  
  # Build covariance matrix from exponential function
  for(i in 1:N) {
    for(j in 1:N) {
      Sigma[i, j] <- A2 * exp(-phi * dist[i, j]) + tau2 * isdiag[i, j]
    }
  }
  
  # priors
  beta0 ~ dnorm(0, sd = 10)
  beta1 ~ dnorm(0, sd = 10)
  tau2 ~ dinvgamma(shape = 2, rate = 1)
  phi ~ dunif(0.01, 5)  # spatial range parameter
  A2 ~ dinvgamma(shape = 2, rate = 1)
})

# Add distance matrix to constants
my.constants <- list(N = 5, dist = dist_matrix)

parameters.to.save <- c("beta0", "beta1", "tau2", "phi", "A2")

# Initial values

initial.values <- list(
  list(alpha = 1, beta = 0.5, A2 = 0.5, tau2 = 0.2, phi = 0.1),
  list(alpha = 1.5, beta = 0.7, A2 = 0.8, tau2 = 0.3, phi = 0.5),
  list(alpha = 1.2, beta = 0.6, A2 = 1.0, tau2 = 0.4, phi = 1.0)
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
          params = "A2")

#we're inferring basically nothing about phi

# Extract posterior means
mean(mcmc.output$chain1[,'beta0'])
mean(mcmc.output$chain1[,'beta'])
mean(mcmc.output$chain1[,'tau2'])
mean(mcmc.output$chain1[,'phi'])

# 95% credible intervals
quantile(mcmc.output$chain1[,'phi'], probs = c(0.025, 0.975))