# Make sure RandomFields and INLA package work
source('header.R')

# Number of points and number of areas
pnum <- c(10, 50, 100)
anum <- c(2, 4)

# Parameters to simulate surface
simus <- list(
  simu1 = data.frame(mu1 = 4, mu0 = 0, nu = 1, nu0 = 1, scl = 0.1, scl0 = 0.1, sig2 = 4, sig20 = 4, sig.err = 1, beta0 = 1, beta1 = 1),
  simu2 = data.frame(mu1 = 4, mu0 = 0, nu = 1, nu0 = 1, scl = 0.5, scl0 = 0.5, sig2 = 4, sig20 = 4, sig.err = 1, beta0 = 0.1, beta1 = 0.1))

# Fit models


for(paramname in names(simus)){
for(anumm in anum){
for(pnumm in pnum){
    
print(pnumm)
print(anumm)
print(paramname)

# Choose parameters
param <- simus[[paramname]]

# "Generate surface", "Take measurements at points and areas", "Fit models", "Calculate errors"
wantplot <- TRUE
source("02-simulation.R")

}}}


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





# Large correlation and many points work better for PS_pointsareas
# scl <- 0.55
# scl0 <- 0.53

# MAE and MSE of Cox_melding are the same: the absolute values of all prediction errors are identical. 


# For pnum = 10, anum = 2
# Based on what I have try, the quality of the estimation of cox melding depends heavily on the pnum. since I just try anum = 2/4, I cannot say that anum does not contribute too much. In addition, it is also sensitive to sampling pattern. 
