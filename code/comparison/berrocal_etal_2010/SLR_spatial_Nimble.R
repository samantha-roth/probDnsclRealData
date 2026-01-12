rm(list=ls())
graphics.off()
library(nimble)
library(MCMCvis)


# Data
my.data <- list(
  y = c(0.932688, 1.554480, 1.798320, 2.377440, 1.341120),
  x = c(0.516, 0.719, 1.219, 2.261, 1.151)
)

# Constants
my.constants <- list(N = 5)

# Locations
locations <- matrix(c(210, 325,
                      92, 6,
                      202, 292,
                      0, 2,
                      192, 0), nrow = 5, ncol = 2, byrow = TRUE)

# Calculate distance matrix
dist_matrix <- as.matrix(dist(locations))

# Create a function to compute exponential covariance matrix
# exp_cov <- function(dist, sigma, phi) {
#   cov_matrix <- sigma^2 * exp(-phi * dist)
#   return(cov_matrix)
# }
# 
# # Function to build the covariance matrix in the model
# buildCovariance <- function(sigma, phi, dist_matrix) {
#   cov_matrix <- sigma^2 * exp(-phi * dist_matrix)
#   return(cov_matrix)
# }

# Add the covariance matrix computation to the model
model <- nimbleCode({
  # likelihood with spatial correlation
  y[1:N] ~ dmnorm(mu[1:N], cov = Sigma[1:N, 1:N])
  
  # linear predictor
  for(i in 1:N) {
    mu[i] <- alpha + beta * x[i]
  }
  
  # Build covariance matrix from exponential function
  for(i in 1:N) {
    for(j in 1:N) {
      Sigma[i, j] <- sigma^2 * exp(-phi * dist[i, j])
    }
  }
  
  # priors
  alpha ~ dnorm(0, sd = 10)
  beta ~ dnorm(0, sd = 10)
  sigma ~ dinvgamma(shape = 2, rate = 1)
  phi ~ dunif(0.01, 5)  # spatial range parameter
})

# Add distance matrix to constants
my.constants <- list(N = 5, dist = dist_matrix)

parameters.to.save <- c("alpha", "beta", "sigma", "phi")

# Initial values
initial.values <- list(
  list(alpha = 0, beta = 1, sigma = 0.5, phi = 0.5),
  list(alpha = 0.5, beta = 1.5, sigma = 1, phi = 1),
  list(alpha = 1, beta = 0.5, sigma = 1.5, phi = 2)
)

# MCMC settings
set.seed(666)
n.iter <- 5000
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
          params = "alpha")

MCMCtrace(object = mcmc.output,
          pdf = FALSE,
          ind = TRUE,
          params = "beta")

MCMCtrace(object = mcmc.output,
          pdf = FALSE,
          ind = TRUE,
          params = "sigma")

MCMCtrace(object = mcmc.output,
          pdf = FALSE,
          ind = TRUE,
          params = "phi")

#we're inferring basically nothing about phi

# Extract posterior means
mean(mcmc.output$chain1[,'alpha'])
mean(mcmc.output$chain1[,'beta'])
mean(mcmc.output$chain1[,'sigma'])
mean(mcmc.output$chain1[,'phi'])

# 95% credible intervals
quantile(mcmc.output$chain1[,'phi'], probs = c(0.025, 0.975))