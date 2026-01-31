rm(list=ls())
graphics.off()

library(nimble)
library(MCMCvis)
library(terra)

setwd("/Users/f007f8t/Documents/GitHub/probDnsclRealData")

#get 10m model run
run10m<- rast("data/Outputs10m/Run_1.asc")
#get grid cells we want
load("data/boxAroundHWMs.10m.RData")
#extract flood heights at grid cells we want
vals10minBds<- c(as.matrix(extract(run10m,coordsinBds.10m)))

#center vals10minBds for simplicity
vals10m.c<- vals10minBds-mean(vals10minBds)

n_loc<- length(vals10minBds)
# ============================================
# STEP 1: Calculate nearest neighbors
# ============================================

# Data
my.data <- list(y= vals10minBds)

# Locations
locations <- coordsinBds.10m

#using euclidean distance
dist_matrix <- as.matrix(dist(locations,method="euclidean"))
n <- nrow(locations)
m <- 4  # Number of nearest neighbors

# Function to find m nearest neighbors for each location
find_nn <- function(dist_matrix, m) {
  n <- nrow(dist_matrix)
  nn_indices <- matrix(0, nrow = n, ncol = m)
  nn_distances <- matrix(0, nrow = n, ncol = m)
  
  for(i in 1:n) {
    # Get distances from location i to all others (excluding itself)
    distances <- dist_matrix[i, -i]
    
    # Find m nearest neighbors
    nn_idx <- order(distances)[1:m]
    
    # Adjust indices to account for exclusion of self
    nn_idx_adjusted <- nn_idx + (nn_idx >= i)
    
    nn_indices[i, ] <- nn_idx_adjusted
    nn_distances[i, ] <- sort(distances)[1:m]
  }
  
  list(indices = nn_indices, distances = nn_distances)
}

nn_structure <- find_nn(dist_matrix, m)
nn_indices <- nn_structure$indices
nn_distances <- nn_structure$distances

# print("Nearest neighbor structure:")
# print("Indices:")
# print(nn_indices)
# print("Distances:")
# print(nn_distances)

# ============================================
# STEP 2: Build NNGP covariance structure
# ============================================

# For each location i, compute conditional covariance given nearest neighbors
# w_i | w_{N(i)} ~ N(mu_i, sigma2_i)
# where N(i) are the nearest neighbors of i

# Function to build NNGP precision (inverse covariance) matrix
build_nngp_precision <- function(nn_indices, nn_distances, sigma2, phi, tau2, m) {
  n <- nrow(nn_indices)
  
  # Distance matrix within neighbors
  neighbor_dist <- array(0, dim = c(n, m, m))
  
  for(i in 1:n) {
    nn_idx <- nn_indices[i, ]
    
    # Distances among neighbors
    for(j1 in 1:m) {
      for(j2 in 1:m) {
        if(j1 == j2) {
          neighbor_dist[i, j1, j2] <- 0
        } else {
          loc1 <- nn_idx[j1]
          loc2 <- nn_idx[j2]
          neighbor_dist[i, j1, j2] <- sqrt(sum((locations[loc1, ] - locations[loc2, ])^2))
        }
      }
    }
  }
  
  list(neighbor_dist = neighbor_dist)
}

# ============================================
# STEP 3: NIMBLE Model with NNGP
# ============================================

model <- nimbleCode({
  # Decompose observed data
  for(i in 1:N) {
    y[i] <- w[i] + z[i]
  }
  
  # Measurement error component
  z[1:N] ~ dmnorm(zeros[1:N], cov = Sigma_meas[1:N, 1:N])
  
  # NNGP for spatial component
  # First location
  w[1] ~ dnorm(mu[1], sd = sqrt(sigma2))
  
  # Locations 2 to N conditioned on nearest neighbors
  for(i in 2:N) {
    # Number of neighbors to condition on
    n_neighbors <- min(i - 1, m)  # Can't have more neighbors than previous locations
    
    # Mean conditional on neighbors
    mu_cond[i] <- mu[i] + sum(beta_nn[i, 1:n_neighbors] * 
                                (w[nn_indices[i, 1:n_neighbors]] - mu[nn_indices[i, 1:n_neighbors]]))
    
    # Conditional variance
    var_cond[i] <- sigma2 - sum(beta_nn[i, 1:n_neighbors]^2 * sigma2)
    
    # Draw from conditional distribution
    w[i] ~ dnorm(mu_cond[i], sd = sqrt(var_cond[i] + 1e-6))
  }
  
  # Compute regression coefficients for NNGP
  for(i in 2:N) {
    n_neighbors <- min(i - 1, m)
    
    for(j in 1:n_neighbors) {
      nn_idx <- nn_indices[i, j]
      # Distance to neighbor
      d_ij <- nn_dist[i, j]
      
      # Correlation between i and neighbor j
      rho_ij <- exp(-phi * d_ij)
      
      # Covariance with neighbors (simplified for NNGP)
      C_ij[i, j] <- sigma2 * rho_ij
    }
    
    # Compute conditional mean coefficients
    # This is a simplified version - full NNGP requires matrix inversion
    for(j in 1:n_neighbors) {
      beta_nn[i, j] <- C_ij[i, j] / sigma2
    }
  }
  
  # Linear predictor
  for(i in 1:N) {
    mu[i] <- overall_mean
  }
  
  # Measurement error covariance (diagonal)
  for(i in 1:N) {
    for(j in 1:N) {
      Sigma_meas[i, j] <- tau2 * isdiag[i, j]
    }
  }
  
  # Priors
  sigma2 ~ dinvgamma(shape = 2, rate = 1)
  tau2 ~ dinvgamma(shape = 2, rate = 1)
  phi ~ dunif(0.01, 10)
})

# ============================================
# STEP 4: Alternative: Simpler NNGP Implementation
# ============================================

# More practical simplified version
model <- nimbleCode({
  # Decompose observed data
  for(i in 1:N) {
    y[i] <- w[i] + z[i]
  }

  # Measurement error
  z[1:N] ~ dmnorm(zeros[1:N], cov = Sigma_meas[1:N, 1:N])

  # Spatial process using full covariance (NNGP approximation)
  # We'll use NNGP by computing approximate covariance
  w[1:N] ~ dmnorm(mu[1:N], cov = Sigma_spatial[1:N, 1:N])

  # Linear predictor
  for(i in 1:N) {
    mu[i] <- overall_mean
  }

  # Build covariance matrix with NNGP approximation
  # For each location, only compute covariance with nearest neighbors
  for(i in 1:N) {
    for(j in 1:N) {
      if(i == j) {
        # Diagonal: marginal variance
        Sigma_spatial[i, j] <- sigma2 + 1e-6  # Small nugget for numerical stability
      } else {
        # Distance between locations i and j
        d_ij <- nn_dist_full[i, j]

        # Exponential covariance only if within nearest neighbor set
        # Otherwise set to 0 (local approximation)
        if(is_neighbor[i, j] == 1) {
          Sigma_spatial[i, j] <- sigma2 * exp(-phi * d_ij)
        } else {
          Sigma_spatial[i, j] <- 0
        }
      }
    }
  }

  # Measurement error covariance
  for(i in 1:N) {
    for(j in 1:N) {
      Sigma_meas[i, j] <- tau2 * isdiag[i, j]
    }
  }

  # Priors
  sigma2 ~ dinvgamma(shape = 2, rate = 1)
  tau2 ~ dinvgamma(shape = 2, rate = 1)
  phi ~ dunif(0.01, 5)
})

# ============================================
# STEP 5: Prepare data and fit model
# ============================================

# Create neighbor indicator matrix
is_neighbor <- matrix(0, nrow = n, ncol = n)
for(i in 1:n) {
  for(j in 1:m) {
    is_neighbor[i, nn_indices[i, j]] <- 1
  }
}

my.data$phi_values <- NULL
my.data$prob_phi <- NULL

isdiag <- diag(n)
zeros <- rep(0, n)

my.constants <- list(
  N = n,
  m = m,
  nn_indices = nn_indices,
  nn_dist = nn_distances,
  nn_dist_full = dist_matrix,
  is_neighbor = is_neighbor,
  isdiag = isdiag,
  zeros = zeros,
  overall_mean= mean(vals10minBds)
)

parameters.to.save <- c("sigma2", "tau2", "phi", "w", "z")

initial.values <- list(
  list(alpha = 1, beta = 0.5, sigma2 = 0.5, tau2 = 0.2, phi = 0.5,
       w = c(1, 1.5, 1.8, 2.4, 1.3), z = c(0, 0, 0, 0, 0)),
  list(alpha = 1.5, beta = 0.7, sigma2 = 0.8, tau2 = 0.3, phi = 1,
       w = c(0.9, 1.6, 1.7, 2.3, 1.4), z = c(0.05, -0.05, 0.05, 0.05, 0)),
  list(alpha = 1.2, beta = 0.6, sigma2 = 1.0, tau2 = 0.4, phi = 2,
       w = c(1.1, 1.4, 1.9, 2.5, 1.2), z = c(-0.05, 0, -0.1, -0.05, 0.05))
)

set.seed(666)
mcmc.output <- nimbleMCMC(code = model,
                          data = my.data,
                          constants = my.constants,
                          inits = initial.values,
                          monitors = parameters.to.save,
                          niter = 5000,
                          nburnin = 1000,
                          nchains = 3)

# Results

proc.time()

MCMCsummary(object = mcmc.output, round = 2)

MCMCtrace(object = mcmc.output,
          pdf = FALSE,
          ind = TRUE,
          params = "phi")