# Make sure RandomFields and INLA package work
source('header.R')

# Number of points and number of areas
pnumm <- 100
anumm <- 2
N= 1
nu = 1
simus <- list(
  simu7 = list(data.frame(mu1 = 0, mu0 = 0, nu = 1, scl = 0.1, scl0 = 0.1, sig2 = 1, sig20 = 1, sig.err = 0.1*1, beta1 = 1, beta0 = 0.05), prior.range = NULL, prior.sigma = NULL)
)
paramname <- 'simu7'
param <- simus[[paramname]][[1]]
prior.range <- simus[[paramname]][["prior.range"]]
prior.sigma <- simus[[paramname]][["prior.sigma"]]
source("01.5-simulation.R")
lr <- fnGenerateSurface(xlim, ylim, by, mu1, mu0, nu, nu0, scl, scl0, sig2, sig20)
r <- lr[1][[1]]
rs <- r
lpa <- fnMeasurementsatPointsAndAreas(pnumm, anumm, r, rs, seed, beta1 = beta1, beta0 = beta0 , sig.err = sig.err)
depoint <- lpa[1][[1]]
dearea <- lpa[2][[1]]
spde.posterior()

res <- fnPredictMeldingPSnoee(depoint = depoint, dearea = dearea, loc.d = loc.d, dppoint = dppoint, mesh = mesh, boundaryregion = boundaryregion)
result <- res[[3]]

matcov <- inlabru::spde.posterior(result, "i", "matern.covariance")
plot(matcov)