source("simulation_heter/header.R")

# Number of points and number of areas
pnum <- c(100, 250)
anum <- c(2,5,10)
N= 50

simus <- list( simu1 = list(data.frame(mu1 = 0, mu0 = 0, nu = 1, scl = 0.1, sig.err = 0.1*1, beta1 = 1, beta0 = 0.05,phi = 0.8), prior.range = c(0.14, 0.8), prior.sigma = c(1, 0.1))
  #   simu2 = list(data.frame(mu1 = 0, mu0 = 0, nu = 1, scl = 0.05, scl0 = 0.05, sig2 = 1, sig20 = 1, sig.err = 0.1*1, beta1 = 1, beta0 = 0.05),prior.range = c(0.99, 0.8), prior.sigma = c(1, 0.1)),
  #   simu3 = list(data.frame(mu1 = 0, mu0 = 0, nu = 1, scl = 0.2, scl0 = 0.2, sig2 = 1, sig20 = 1, sig.err = 0.1*1, beta1 = 1, beta0 = 0.05), prior.range = c(0.38, 0.8), prior.sigma = c(1, 0.1)),
  #   simu4 = list(data.frame(mu1 = 0, mu0 = 0, nu = 1, scl = 0.1, scl0 = 0.1, sig2 = 1, sig20 = 1, sig.err = 0.1*1, beta1 = 0, beta0 = 0.05), prior.range = c(0.14, 0.8), prior.sigma = c(1, 0.1)),
  #   simu5 = list(data.frame(mu1 = 0, mu0 = 0, nu = 1, scl = 0.05, scl0 = 0.05, sig2 = 1, sig20 = 1, sig.err = 0.1*1, beta1 = 0, beta0 = 0.05), prior.range = c(0.99, 0.8), prior.sigma = c(1, 0.1)),
  # simu6 = list(data.frame(mu1 = 0, mu0 = 0, nu = 1, scl = 0.2, scl0 = 0.2, sig2 = 1, sig20 = 1, sig.err = 0.1*1, beta1 = 0, beta0 = 0.05),prior.range = c(0.38, 0.8), prior.sigma = c(1, 0.1)),
  # simu7 = list(data.frame(mu1 = 0, mu0 = 0, nu = 1, scl = 0.1, scl0 = 0.1, sig2 = 1, sig20 = 1, sig.err = 0.1*1, beta1 = 1, beta0 = 0.05), prior.range = NULL, prior.sigma = NULL),
  # simu8 = list(data.frame(mu1 = 0, mu0 = 0, nu = 1, scl = 0.05, scl0 = 0.05, sig2 = 1, sig20 = 1, sig.err = 0.1*1, beta1 = 1, beta0 = 0.05),prior.range = NULL, prior.sigma = NULL),
  # simu9 = list(data.frame(mu1 = 0, mu0 = 0, nu = 1, scl = 0.2, scl0 = 0.2, sig2 = 1, sig20 = 1, sig.err = 0.1*1, beta1 = 1, beta0 = 0.05), prior.range = NULL, prior.sigma = NULL),
  # simu10 = list(data.frame(mu1 = 0, mu0 = 0, nu = 1, scl = 0.1, scl0 = 0.1, sig2 = 1, sig20 = 1, sig.err = 0.1*1, beta1 = 0, beta0 = 0.05), prior.range = NULL, prior.sigma = NULL),
  # simu11 = list(data.frame(mu1 = 0, mu0 = 0, nu = 1, scl = 0.05, scl0 = 0.05, sig2 = 1, sig20 = 1, sig.err = 0.1*1, beta1 = 0, beta0 = 0.05), prior.range = NULL, prior.sigma = NULL),
  # simu12 = list(data.frame(mu1 = 0, mu0 = 0, nu = 1, scl = 0.2, scl0 = 0.2, sig2 = 1, sig20 = 1, sig.err = 0.1*1, beta1 = 0, beta0 = 0.05),prior.range = NULL, prior.sigma = NULL),
  # simu13 = list(data.frame(mu1 = 0, mu0 = 0, nu = 1, scl = 0.05, scl0 = 0.05, sig2 = 1, sig20 = 1, sig.err = 0.1*1, beta1 = 0, beta0 = 0),prior.range = NULL, prior.sigma = NULL)
)
#
s <- c(0.05,0.1, 0.2)
kappa <- sqrt(2) / s
theta <- 2/s^2
simus <- list(
  simu7 = list(data.frame(mu1 = 0, mu0 = 0, nu = 1, scl = 0.1, scl0 = 0.1, sig2 = 1, sig20 = 1, sig.err = 0.1*1, beta1 = 1, beta0 = 0.05), prior.range = NULL, prior.sigma = NULL),
  # simu8 = list(data.frame(mu1 = 0, mu0 = 0, nu = 1, scl = 0.05, scl0 = 0.05, sig2 = 1, sig20 = 1, sig.err = 0.1*1, beta1 = 1, beta0 = 0.05),prior.range = NULL, prior.sigma = NULL),
  # simu9 = list(data.frame(mu1 = 0, mu0 = 0, nu = 1, scl = 0.2, scl0 = 0.2, sig2 = 1, sig20 = 1, sig.err = 0.1*1, beta1 = 1, beta0 = 0.05), prior.range = NULL, prior.sigma = NULL)
  #  simu10 = list(data.frame(mu1 = 0, mu0 = 0, nu = 1, scl = 0.1, scl0 = 0.1, sig2 = 1, sig20 = 1, sig.err = 0.1*1, beta1 = 0, beta0 = 0.05), prior.range = NULL, prior.sigma = NULL),
  # simu11 = list(data.frame(mu1 = 0, mu0 = 0, nu = 1, scl = 0.05, scl0 = 0.05, sig2 = 1, sig20 = 1, sig.err = 0.1*1, beta1 = 0, beta0 = 0.05), prior.range = NULL, prior.sigma = NULL),
  # simu12 = list(data.frame(mu1 = 0, mu0 = 0, nu = 1, scl = 0.2, scl0 = 0.2, sig2 = 1, sig20 = 1, sig.err = 0.1*1, beta1 = 0, beta0 = 0.05),prior.range = NULL, prior.sigma = NULL)
)

cl <- makeClusterPSOCK(availableCores())
plan(cluster, workers = cl)



# Fit models 

future_lapply(1:20, FUN = fnSimulate, simus = simus, future.seed = T)

fnSimulate <- function(index, simus, ...){
  source('header.R')
  source('01-simulation.R')
  for(paramname in names(simus)){
    param <- simus[[paramname]][[1]]
    prior.range <- simus[[paramname]][["prior.range"]]
    prior.sigma <- simus[[paramname]][["prior.sigma"]]
    source("01.5-simulation.R", local= T)
    for(anumm in anum){
      for(pnumm in pnum){
        # Choose parameters
        # "Generate surface", "Take measurements at points and areas", "Fit models", "Calculate errors"
        wantplot <- F
        source("02-simulation.R", local= T)
      }}
  }
}
