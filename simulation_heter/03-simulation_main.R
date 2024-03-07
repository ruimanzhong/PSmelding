# Make sure RandomFields and INLA package work
source('simulation_heter/header.R')
# Number of points and number of areas
pnum <- c(100, 250)
anum <- c(2,5,10)
N= 50

simus <- list(
  # simu7 = list(data.frame(mu1 = 0,nu = 1, scl = 0.1, sig2 = 1, sig.err = 0.1*1, beta1 = 1 , beta0 = 0.05,phi = 0.8), prior.range = NULL, prior.sigma = NULL),
  # simu8 = list(data.frame(mu1 = 0, nu = 1, scl = 0.05, sig2 = 1, sig.err = 0.1*1, beta1 = 1 , beta0 = 0.05,phi = 0.8),prior.range = NULL, prior.sigma = NULL),
  # simu9 = list(data.frame(mu1 = 0,nu = 1, scl = 0.2, sig2 = 1, sig.err = 0.1*1, beta1 = 1 , beta0 = 0.05,phi = 0.8), prior.range = NULL, prior.sigma = NULL),
  # simu10 = list(data.frame(mu1 = 0, nu = 1, scl = 0.1, sig2 = 1, sig.err = 0.1*1, beta1 = 0 , beta0 = 0.05,phi = 0.8), prior.range = NULL, prior.sigma = NULL),
  # simu11 = list(data.frame(mu1 = 0, nu = 1, scl = 0.05,  sig2 = 1, sig.err = 0.1*1, beta1 = 0 , beta0 = 0.05,phi = 0.8), prior.range = NULL, prior.sigma = NULL),
  simu12 = list(data.frame(mu1 = 0, nu = 1, scl = 0.2, sig2 = 1, sig.err = 0.1*1, beta1 = 0 , beta0 = 0.05,phi = 0.8),prior.range = NULL, prior.sigma = NULL)
)
#
library(RhpcBLASctl)
blas_get_num_procs()
blas_set_num_threads(6)
cl <- makeClusterPSOCK(6)
plan(cluster, workers = cl)
# Fit models 
# 51
debugr_switchOn()
future_lapply(1, FUN = fnSimulate, simus = simus, future.seed = T)

fnSimulate <- function(index, simus, ...){
  source('simulation_heter/header.R')
  source('simulation_heter/01-simulation.R')
  for(paramname in names(simus)){
    param <- simus[[paramname]][[1]]
    prior.range <- simus[[paramname]][["prior.range"]]
    prior.sigma <- simus[[paramname]][["prior.sigma"]]
    source("simulation_heter/01.5-simulation.R", local= T)
    for(anumm in anum){
      for(pnumm in pnum){
        # Choose parameters
        # "Generate surface", "Take measurements at points and areas", "Fit models", "Calculate errors"
        wantplot <- F
        source("simulation_heter/02-simulation.R", local= T)
      }}
  }
}
