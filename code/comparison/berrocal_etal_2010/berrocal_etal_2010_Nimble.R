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
est_beta0<- as.numeric(SLR_fit$coefficients[1])
est_beta1<- as.numeric(SLR_fit$coefficients[2])

mse<- mean(SLR_fit$residuals^2) #to inform tau2

locations<- HWMlocs

# Calculate distance matrix using manhattan distance because it's an exp cov function
dist_matrix <- as.matrix(dist(locations,method= "manhattan"))

# ################################################################################
# # base version of berrocal et al model uninformed by empirical semivariogram
# 
# model0 <- nimbleCode({
#   
#   #y is now the sum of multiple gaussian processes
#   for(i in 1:N) {
#     y[i] <- beta0 + beta1 * x[i] + beta0_s[i] + beta1_s[i] * x[i] + epsilon[i] 
#     beta0_s[i]<- A11 * w0[i]
#     beta1_s[i]<- A21 * w0[i] + A22 * w1[i]
#     epsilon[i] ~ dnorm(0, var = tau2)
#   }
#   
#   #zero-mean, unit variance spatial processes
#   w0[1:N] ~ dmnorm(zeros[1:N], cov = Sigma0[1:N, 1:N])
#   w1[1:N] ~ dmnorm(zeros[1:N], cov = Sigma1[1:N, 1:N])
#   
#   # Build spatial covariance matrix with exponential covariance function
#   for(i in 1:N) {
#     for(j in 1:N) {
#       Sigma0[i, j] <- exp(-phi0_vals[phi0_ind] * dist[i, j])
#       Sigma1[i, j] <- exp(-phi1_vals[phi1_ind] * dist[i, j])
#     }
#   }
#   
#   # priors
#   beta0 ~ dnorm(mu_beta0, sd = 2)
#   beta1 ~ dnorm(mu_beta1, sd = 2)
#   tau2 ~ dinvgamma(shape = 1, rate = 1)
#   #phi0 ~ dcat(prob = rep(1/5, 5))  # spatial range parameter
#   phi0_ind ~ dcat(prob = prob_phi0[1:n_phi0])
#   phi1_ind ~ dcat(prob = prob_phi1[1:n_phi1])
#   # phi0 ~ dunif(0.01, 5)
#   # phi1 ~ dunif(0.01, 5)
#   A11 ~ dlnorm(0,sdlog=1)
#   A22 ~ dlnorm(0,sdlog=1)
#   A21 ~ dnorm(0,sd=1)
# })
# 
# 
# #Define discrete uniform distributions for phi0 and phi1
# phi0_vals <- seq(1, 5, by = 1)
# n_phi0 <- length(phi0_vals)
# prob_phi0 <- rep(1/n_phi0, n_phi0)
# 
# phi1_vals <- seq(1, 5, by = 1)
# n_phi1 <- length(phi1_vals)
# prob_phi1 <- rep(1/n_phi1, n_phi1)
# 
# # Add distance matrix to constants
# my.constants <- list(N = 5, 
#                      dist = dist_matrix,
#                      n_phi0 = n_phi0,
#                      n_phi1 = n_phi1,
#                      mu_beta0 = est_beta0,
#                      mu_beta1 = est_beta1
# )
# 
# #generate random initial values for all parameters
# initial.values.function0 <- function() {
#   return(list(beta0 = rnorm(1, mean=0.75, sd=2),
#               beta1 = rnorm(1, mean=0.72, sd=2), 
#               tau2 = rinvgamma(1, shape= 1, rate=1),
#               phi0_ind = sample(1:n_phi0, size= 1),
#               phi1_ind = sample(1:n_phi1, size= 1),
#               A21 = rnorm(1, mean=0, sd=1),
#               A11 = rlnorm(1, meanlog = 0, sdlog = 1),
#               A22 = rlnorm(1, meanlog = 0, sdlog = 1)))
# }


################################################################################
#version of berrocal et al model informed by empirical semivariogram

#load estimates for the parameters of the exponential covariance function
load("data/est_expcov_pars")

model1 <- nimbleCode({
  
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
  beta0 ~ dnorm(mu_beta0, sd = 1)
  beta1 ~ dnorm(mu_beta1, sd = 1)
  tau2 ~ dinvgamma(shape = shape_tau2, rate = 1)
  #phi0 ~ dcat(prob = rep(1/5, 5))  # spatial range parameter
  phi0_ind ~ dcat(prob = prob_phi0[1:n_phi0])
  phi1_ind ~ dcat(prob = prob_phi1[1:n_phi1])
  # phi0 ~ dunif(0.01, 5)
  # phi1 ~ dunif(0.01, 5)
  A11 ~ dlnorm(mu_A11,sdlog=3)
  A22 ~ dlnorm(0,sdlog=3)
  A21 ~ dnorm(0,sd=3)
})


max_phi0<- max_phi1<- ceiling(est_phi)

#Define discrete uniform distributions for phi0 and phi1
phi0_vals <- seq(1, max_phi0, by = 1)
n_phi0 <- length(phi0_vals)
prob_phi0 <- rep(1/n_phi0, n_phi0)

phi1_vals <- seq(1, max_phi1, by = 1)
n_phi1 <- length(phi1_vals)
prob_phi1 <- rep(1/n_phi1, n_phi1)

#define mu_A11 so that the mean of the lognormal distribution is equal to 
#the square root of the partial sill. 
#then the variance of w0 will be equal to the estimated partial sill

#also make the make the maximum allowable value of phi0 and phi1 based on
#the ceiling of the fit value in the empirical semivariogram.
#because the strength of the evidence for spatial correlation that strong is weak.

#the shape parameter in the prior tau2 is set so that the mean is equal to 
#the estimated nugget and beta (rate)=1

# Add distance matrix to constants
my.constants1 <- list(N = 5, 
                     dist = dist_matrix,
                     n_phi0 = n_phi0,
                     n_phi1 = n_phi1,
                     mu_A11 = log(sqrt(est_partial_sill))-9/2,
                     mu_beta0 = est_beta0,
                     mu_beta1 = est_beta1,
                     shape_tau2 = 1/(est_nugget)+1
) 

#generate random initial values for all parameters
initial.values.function1 <- function() {
  return(list(beta0 = rnorm(1, mean=est_beta0, sd=1),
              beta1 = rnorm(1, mean=est_beta1, sd=1), 
              tau2 = rinvgamma(1, shape= 1/(est_nugget)+1, rate=1),
              phi0_ind = sample(1:n_phi0, size= 1),
              phi1_ind = sample(1:n_phi1, size= 1),
              A21 = rnorm(1, mean=0, sd=3),
              A11 = rlnorm(1, meanlog = log(sqrt(est_partial_sill))-9/2, sdlog = 3),
              A22 = rlnorm(1, meanlog = 0, sdlog = 3)))
}


################################################################################

# Data
my.data <- list(
  y = obs,
  x = wsh.10m,
  prob_phi0 = prob_phi0,
  phi0_vals = phi0_vals,
  prob_phi1 = prob_phi1,
  phi1_vals = phi1_vals
)

parameters.to.save <- c("beta0", "beta1", "tau2", "phi0_ind", "phi1_ind", "A11", "A21", "A22")


# MCMC settings
set.seed(666)
n.iter <- 10000
n.burnin <- 1000
n.chains <- 3

# Fit model
mcmc.output <- nimbleMCMC(code = model1,
                          data = my.data,
                          constants = my.constants1,
                          inits = initial.values.function1,
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


save(mcmc.output,file="data/berrocal_mcmc.output")

#model 1: 
#we're learning basically nothing about beta0, beta1, phi0, phi1, and A21
#A11 and A22 estimates are unstable; 
#tau2 is reinforced as being extremely small
#A11 and A22 have some very large outliers


# # Extract posterior means
# mean(mcmc.output$chain1[,'beta0'])
# mean(mcmc.output$chain1[,'beta1'])
# mean(mcmc.output$chain1[,'tau2'])
# mean(mcmc.output$chain1[,'sigma2'])
# mean(mcmc.output$chain1[,'phi0_ind'])
# mean(mcmc.output$chain1[,'phi1_ind'])
# 
# #extract posterior medians
# median(mcmc.output$chain1[,'beta0'])
# median(mcmc.output$chain1[,'beta1'])
# median(mcmc.output$chain1[,'tau2'])
# median(mcmc.output$chain1[,'sigma2'])
# median(mcmc.output$chain1[,'phi0_ind'])
# median(mcmc.output$chain1[,'phi1_ind'])
# 
# # 95% credible intervals
# quantile(mcmc.output$chain1[,'phi0_ind'], probs = c(0.025, 0.975))