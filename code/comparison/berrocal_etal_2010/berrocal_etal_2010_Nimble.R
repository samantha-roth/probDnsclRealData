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

locations<- HWMlocs

# Calculate distance matrix
dist_matrix <- as.matrix(dist(locations))

# Add the covariance matrix computation to the model
model <- nimbleCode({
  
  #y is now the sum of multiple gaussian processes
  for(i in 1:N) {
    y[i] <- beta0 + beta1 * x[i] + beta0_s[i] + beta1_s[i] * x[i] + epsilon[i] 
    beta0_s[i]<- A11 * w0[i]
    beta1_s[i]<- A21 * w0[i] + A22 * w1[i]
    epsilon[i] ~ dnorm(0, var = tau2)
  }
  
  #zero-mean, unit variance spatial processes
  w0[1:N] ~ dmnorm(zeros[1:N], cov = Sigma0[1:N, 1:N])
  w1[1:N] ~ dmnorm(zeros[1:N], cov = Sigma1[1:N, 1:N])
  
  # Build spatial covariance matrix with exponential covariance function
  for(i in 1:N) {
    for(j in 1:N) {
      Sigma0[i, j] <- exp(-phi0_vals[phi0_ind] * dist[i, j])
      Sigma1[i, j] <- exp(-phi1_vals[phi1_ind] * dist[i, j])
    }
  }
  
  # priors
  beta0 ~ dnorm(0.75, sd = 2)
  beta1 ~ dnorm(0.72, sd = 2)
  tau2 ~ dinvgamma(shape = 1, rate = 1)
  #phi0 ~ dcat(prob = rep(1/5, 5))  # spatial range parameter
  phi0_ind ~ dcat(prob = prob_phi0[1:n_phi0])
  phi1_ind ~ dcat(prob = prob_phi1[1:n_phi1])
  # phi0 ~ dunif(0.01, 5)
  # phi1 ~ dunif(0.01, 5)
  A11 ~ dlnorm(0,sdlog=1)
  A22 ~ dlnorm(0,sdlog=1)
  A21 ~ dnorm(0,sd=1)
})


#Define discrete uniform distributions for phi0 and phi1
phi0_vals <- seq(1, 5, by = 1)
n_phi0 <- length(phi0_vals)
prob_phi0 <- rep(1/n_phi0, n_phi0)

phi1_vals <- seq(1, 5, by = 1)
n_phi1 <- length(phi1_vals)
prob_phi1 <- rep(1/n_phi1, n_phi1)

# Data
my.data <- list(
  y = obs,
  x = wsh.10m,
  prob_phi0 = prob_phi0,
  phi0_vals = phi0_vals,
  prob_phi1 = prob_phi1,
  phi1_vals = phi1_vals
)

# Add distance matrix to constants
my.constants <- list(N = 5, 
                     dist = dist_matrix,
                     n_phi0 = n_phi0,
                     n_phi1 = n_phi1
)

parameters.to.save <- c("beta0", "beta1", "tau2", "phi0_ind", "phi1_ind", "A11", "A21", "A22")

# Initial values

# initial.values <- list(
#   list(beta0 = 0.75, beta1 = 0.72, tau2 = 0.04, phi0_ind = 1, phi1_ind= 1, A11 = 0, A21 =0, A22= 0),
#   list(beta0 = 1, beta1 = 0.5, tau2 = 0.02, phi0_ind = 2, phi1_ind= 4, A11 = 1, A21 =.5, A22= -1),
#   list(beta0 = 0.5, beta1 = 0.8, tau2 = 0.1, phi0_ind = 4, phi1_ind= 2, A11 = -1, A21 =-.5, A22= 1)
# )

#generate random initial values for all parameters
initial.values.function <- function() {
  return(list(beta0 = rnorm(1, mean=0.75, sd=2),
              beta1 = rnorm(1, mean=0.72, sd=2), 
              tau2 = rinvgamma(1, shape= 1, scale=1),
              phi0_ind = sample(1:n_phi0, size= 1),
              phi1_ind = sample(1:n_phi1, size= 1),
              A21 = rnorm(1, mean=0, sd=1),
              A11 = rlnorm(1, meanlog = 0, sdlog = 1),
              A22 = rlnorm(1, meanlog = 0, sdlog = 1)))
}

# MCMC settings
set.seed(666)
n.iter <- 10000
n.burnin <- 1000
n.chains <- 3

# Fit model
mcmc.output <- nimbleMCMC(code = model,
                          data = my.data,
                          constants = my.constants,
                          inits = initial.values.function,
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
          params = "phi0_ind")

MCMCtrace(object = mcmc.output,
          pdf = FALSE,
          ind = TRUE,
          params = "phi1_ind")

MCMCtrace(object = mcmc.output,
          pdf = FALSE,
          ind = TRUE,
          params = "A11")

MCMCtrace(object = mcmc.output,
          pdf = FALSE,
          ind = TRUE,
          params = "A21")

MCMCtrace(object = mcmc.output,
          pdf = FALSE,
          ind = TRUE,
          params = "A22")

#we're inferring basically nothing about phi
#also sigma2 and tau2 are unstable

# Extract posterior means
mean(mcmc.output$chain1[,'beta0'])
mean(mcmc.output$chain1[,'beta1'])
mean(mcmc.output$chain1[,'tau2'])
mean(mcmc.output$chain1[,'sigma2'])
mean(mcmc.output$chain1[,'phi0_ind'])
mean(mcmc.output$chain1[,'phi1_ind'])

#extract posterior medians
median(mcmc.output$chain1[,'beta0'])
median(mcmc.output$chain1[,'beta1'])
median(mcmc.output$chain1[,'tau2'])
median(mcmc.output$chain1[,'sigma2'])
median(mcmc.output$chain1[,'phi0_ind'])
median(mcmc.output$chain1[,'phi1_ind'])

# 95% credible intervals
quantile(mcmc.output$chain1[,'phi0_ind'], probs = c(0.025, 0.975))