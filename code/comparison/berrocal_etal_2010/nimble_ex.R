
rm(list=ls())
graphics.off()

library(nimble)
library(MCMCvis)

model <- nimbleCode({
  # likelihood
  survived ~ dbinom(theta, released)
  # prior
  theta ~ dunif(0, 1)
  # derived quantity
  lifespan <- -1/log(theta)
})

model

my.data <- list(released = 57, survived = 19)

parameters.to.save <- c("theta", "lifespan")

init1 <- list(theta = 0.1)
init2 <- list(theta = 0.5)
init3 <- list(theta = 0.9)
initial.values <- list(init1, init2, init3)
initial.values

my.seed <- 666
set.seed(my.seed)

n.iter <- 5000
n.burnin <- 1000
n.chains <- 3

mcmc.output <- nimbleMCMC(code = model,
                          data = my.data,
                          inits = initial.values,
                          monitors = parameters.to.save,
                          niter = n.iter,
                          nburnin = n.burnin,
                          nchains = n.chains)


str(mcmc.output)

head(mcmc.output$chain1)
dim(mcmc.output$chain1)

mean(mcmc.output$chain1[,'theta'])

quantile(mcmc.output$chain1[,'theta'], probs = c(2.5, 97.5)/100)

hist(mcmc.output$chain1[,"theta"])

MCMCsummary(object = mcmc.output, round = 2)

MCMCtrace(object = mcmc.output,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          params = "theta")