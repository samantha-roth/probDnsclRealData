rm(list=ls())
graphics.off()

library(terra)
library(nimble)
library(MCMCvis)


setwd("/Users/f007f8t/Documents/GitHub/probDnsclRealData")

#load 10m flood projection
run10m<- rast("data/Outputs10m/Run_1.asc")

#load all 10m grid cell centroids
load("data/coords.10m.RData")

#load 10m inds we want to focus on
load("data/goodinds_aboveriver")

locations_c<- coords.10m[goodinds_aboveriver,]
z_c<- terra::values(run10m)[goodinds_aboveriver]


################################################################################
############################NO SPATIAL CORRELATION##############################
################################################################################

model_c0 <- nimbleCode({ 
  # likelihood
  
  for(i in 1:N_c){
    z_c[i] ~ dnorm(mu_c[i],var= tau2_c)
  }
  # #zero-mean, unit variance spatial processes
  # z_c[1:N_c] ~ dmnorm(mu_c[1:N_c], cov = Sigma_c[1:N_c, 1:N_c])
  mu_c[1:N_c] <- G_c[1:N_c, 1:N_b] %*% y_b[1:N_b]
  
  # # Build spatial covariance matrix with exponential covariance function
  # for(i in 1:N_c) {
  #   for(j in 1:N_c) {
  #     Sigma_c[i, j] <- tau2_c * isdiag[i, j]
  #   }
  # }
  
  for(k in 1:N_b) {
    y_b[k] ~ dnorm(0, sd = 5) #the five 5m cells that correspond to HWM locations
  }
  
  tau2_c ~ dinvgamma(shape = 2, scale = 1)
})

N_c = length(z_c)
N_b= 4*N_c
isdiag <- diag(N_c)

#generate random initial values for all parameters
initial.values.function_c0 <- function() {
  return(list(y_b = rnorm(N_b, mean=0, sd=5),
              tau2_c= rinvgamma(1, shape= 2, rate=1)))
}


G_c<- matrix(0, nrow=length(z_c), ncol= 4*length(z_c))
for(i in 1:length(z_c)){
  G_c[i,((i-1)*4+1):(i*4)]<- 1/4
}


my.constants_c0 <- list(N_c = N_c, 
                        N_b = N_b,
                        G_c = G_c, 
                        isdiag = isdiag) 

# Data
my.data_c0 <- list(
  z_c = z_c
)

parameters.to.save_c0 <- c("tau2_c", "y_b")

# MCMC settings
set.seed(666)
n.iter <- 4100
n.burnin <- 100
n.chains <- 1

start<- proc.time()

# Fit model
mcmc.output_c0 <- nimbleMCMC(code = model_c0,
                             data = my.data_c0,
                             constants = my.constants_c0,
                             inits = initial.values.function_c0,
                             monitors = parameters.to.save_c0,
                             niter = n.iter,
                             nburnin = n.burnin,
                             nchains = n.chains)

end<- proc.time()
mcmc_time_c0<- as.numeric(end[3]-start[3])

save(mcmc_time_c0,file="data/wb_onlyZc_mcmc_time_c0")
save(mcmc.output_c0,file="data/wb_onlyZc_mcmc_output_c0")

# Examine results
MCMCsummary(object = mcmc.output_c0, round = 2)

MCMCtrace(object = mcmc.output_c0,
          pdf = FALSE,
          ind = TRUE,
          params = "tau2_c")

y_b_mat_c0<- mcmc.output_c0[,2:ncol(mcmc.output_c0)]

set.seed(10)
rand_inds<- sample(1:ncol(y_b_mat_c0),10)

for(i in 1:length(rand_inds)){
  print(plot(1:nrow(y_b_mat_c0),y_b_mat_c0[,rand_inds[i]],type="l"))
}


################################################################################
########################WITH SPATIAL CORRELATION################################
################################################################################


model_c1 <- nimbleCode({ 
  # likelihood
  #zero-mean, unit variance spatial processes
  z_c[1:N_c] ~ dmnorm(mu_c[1:N_c], cov = Sigma_c[1:N_c, 1:N_c])
  mu_c[1:N_c] <- G_c[1:N_c, 1:N_b] %*% y_b[1:N_b]
  
  # Build spatial covariance matrix with exponential covariance function
  for(i in 1:N_c) {
    for(j in 1:N_c) {
      Sigma_c[i, j] <- tau2_c * isdiag[i, j]+ exp(-phi* dist[i, j])
    }
  }
  
  for(k in 1:N_b) {
    y_b[k] ~ dnorm(0, sd = 5) #the five 5m cells that correspond to HWM locations
  }
  
  phi ~ dunif(0.01, 10)
  tau2_c ~ dinvgamma(shape = 2, scale = 1)
})


# #Define discrete uniform distribution for phi
# max_phi<- 10
# phi_vals <- seq(1, max_phi, by = 1)
# n_phi <- length(phi_vals)
# prob_phi <- rep(1/n_phi, n_phi)

N_c = length(z_c)
N_b= 4*N_c
isdiag <- diag(N_c)
zeros <- rep(0, N_c)

#generate random initial values for all parameters
initial.values.function_c1 <- function() {
  return(list(y_b = rnorm(N_b, mean=0, sd=5),
              tau2_c= rinvgamma(1, shape= 2, rate=1),
              phi = runif(1, 0.01, 10)))
}

# Calculate distance matrix using manhattan distance because it's an exp cov function
dist_matrix_manhattan <- as.matrix(dist(locations_c,method= "manhattan"))


G_c<- matrix(0, nrow=length(z_c), ncol= 4*length(z_c))
for(i in 1:length(z_c)){
  G_c[i,((i-1)*4+1):(i*4)]<- 1/4
}


my.constants_c1 <- list(N_c = N_c, 
                        N_b = N_b,
                        dist = dist_matrix_manhattan,
                        G_c = G_c, 
                        isdiag = isdiag) 

# Data
my.data_c1 <- list(
  z_c = z_c
)

parameters.to.save_c1 <- c("tau2_c", "phi", "y_b")

# MCMC settings
set.seed(666)
n.iter <- 4100
n.burnin <- 100
n.chains <- 3

start<- proc.time()

# Fit model
mcmc.output_c1 <- nimbleMCMC(code = model_c1,
                          data = my.data_c1,
                          constants = my.constants_c1,
                          inits = initial.values.function_c1,
                          monitors = parameters.to.save_c1,
                          niter = n.iter,
                          nburnin = n.burnin,
                          nchains = n.chains)

end<- proc.time()
mcmc_time_c1<- as.numeric(end[3]-start[3])

save(mcmc_time_c1,file="data/wb_onlyZc_mcmc_time_c1")
save(mcmc_output_c1,file="data/wb_onlyZc_mcmc_output_c1")

# Examine results
MCMCsummary(object = mcmc.output_c1, round = 2)
