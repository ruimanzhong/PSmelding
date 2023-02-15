# Make sure RandomFields and INLA package work
source('header.R')

# Number of points and number of areas
pnum <- c(10, 50 , 100)
anum <- c(1,2,4, 10)
N= 100



# 3 value 
# Parameters to simulate surface
simus <- list(
  simu1 = list(data.frame(mu1 = 4, mu0 = 0, nu = 1, scl = 0.1, scl0 = 0.1, sig2 = 4, sig20 = 4, sig.err = 0.1*4, beta1 = 1), prior.range = c(0.05, 0.1), prior.sigma = c(1, 0.9)),
  simu2 = list(data.frame(mu1 = 4, mu0 = 0, nu = 1, scl = 0.1, scl0 = 0.1, sig2 = 4, sig20 = 4, sig.err = 0.1*4, beta1 = 0.5), prior.range = c(0.05, 0.1), prior.sigma = c(1, 0.9)),
  simu3 = list(data.frame(mu1 = 4, mu0 = 0, nu = 1, scl = 0.05, scl0 = 0.05, sig2 = 4, sig20 = 4, sig.err = 0.1*4, beta1 = 1),prior.range = c(0.02, 0.1), prior.sigma = c(1, 0.9)),
  simu4 = list(data.frame(mu1 = 4, mu0 = 0, nu = 1, scl = 0.05, scl0 = 0.05, sig2 = 4, sig20 = 4, sig.err = 0.1*4, beta1 = 0.5),prior.range = c(0.02, 0.1), prior.sigma = c(1, 0.9)),
  simu5 = list(data.frame(mu1 = 4, mu0 = 0, nu = 1, scl = 0.2, scl0 = 0.2, sig2 = 4, sig20 = 4, sig.err = 0.1*4, beta1 = 1), prior.range = c(0.1, 0.1), prior.sigma = c(1, 0.9)),
  simu6 = list(data.frame(mu1 = 4, mu0 = 0, nu = 1, scl = 0.2, scl0 = 0.2, sig2 = 4, sig20 = 4, sig.err = 0.1*4, beta1 = 0.5), prior.range = c(0.1, 0.1), prior.sigma = c(1, 0.9)),
  simu7 = list(data.frame(mu1 = 4, mu0 = 0, nu = 1, scl = 0.1, scl0 = 0.1, sig2 = 10, sig20 = 10, sig.err = 0.1*10, beta1 = 1), prior.range = c(0.05, 0.1), prior.sigma = c(3, 0.9)),
  simu8 = list(data.frame(mu1 = 4, mu0 = 0, nu = 1, scl = 0.1, scl0 = 0.1, sig2 = 10, sig20 = 10, sig.err = 0.1*10, beta1 = 0.5), prior.range = c(0.05, 0.1), prior.sigma = c(3, 0.9)),
  simu9 = list(data.frame(mu1 = 4, mu0 = 0, nu = 1, scl = 0.05, scl0 = 0.05, sig2 = 10, sig20 = 10, sig.err = 0.1*10, beta1 = 1),prior.range = c(0.02, 0.1), prior.sigma = c(3, 0.9)),
  simu10 = list(data.frame(mu1 = 4, mu0 = 0, nu = 1, scl = 0.05, scl0 = 0.05, sig2 = 10, sig20 = 10, sig.err = 0.1*10, beta1 = 0.5),prior.range = c(0.02, 0.1), prior.sigma = c(3, 0.9)),
  simu11 = list(data.frame(mu1 = 4, mu0 = 0, nu = 1, scl = 0.2, scl0 = 0.2, sig2 = 10, sig20 = 10, sig.err = 0.1*10, beta1 = 1), prior.range = c(0.1, 0.1), prior.sigma = c(3, 0.9)),
  simu12 = list(data.frame(mu1 = 4, mu0 = 0, nu = 1, scl = 0.2, scl0 = 0.2, sig2 = 10, sig20 = 10, sig.err = 0.1*10, beta1 = 0.5), prior.range = c(0.1, 0.1), prior.sigma = c(3, 0.9))
  )

cl <- makeCluster(6)
clusterSetRNGStream(cl, iseed = 1234)

# Fit models 
# 51
plan(sequential)
future_lapply(52:N, FUN = fnSimulate, simus = simus, future.seed = TRUE)

fnSimulate <- function(index, simus, ...){
  for(paramname in names(simus)){
    for(anumm in anum){
      for(pnumm in pnum){
        print(pnumm)
        print(anumm)
        print(index)
        
        # Choose parameters
        param <- simus[[paramname]][[1]]
        prior.range <- simus[[paramname]][["prior.range"]]
        prior.sigma <- simus[[paramname]][["prior.sigma"]]
        index <- index
        print(param)
        # "Generate surface", "Take measurements at points and areas", "Fit models", "Calculate errors"
        wantplot <- F
        source("02-simulation.R", local= T)
        
      }}
  }
}



#################################
#################################
#################################

# Read files
mmse <- NULL
mmae <- NULL
for(paramname in names(simus)){
for(anumm in anum){
for(pnumm in pnum){
filesave <- paste0(paramname, "-", "NPoints", pnumm, "NAreas", anumm)
  
vecmse <- read.csv(paste0("results/MSE", filesave, ".csv"))[[1]]
vecmae <- read.csv(paste0("results/MAE", filesave, ".csv"))[[1]]

mmse <- rbind(mmse, c(pnumm, anumm, vecmse))
mmae <- rbind(mmae, c(pnumm, anumm, vecmae))

}}}

mmse
mmae

lapply(1:N, FUN = fnSimulate, simus = simus)
