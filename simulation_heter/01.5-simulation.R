#Parameters
nu = 1
mu1 <- param$mu1
scl <- param$scl
sig.err <- param$sig.err
beta1 <- param$beta1
beta0 <- param$beta0
phi <- param$phi
prior.sigma <- prior.sigma
prior.range <- prior.range
print(is.null(index))
# Generate surface
print("Generate surface")

lr <- fnGenerateSurface_heter(xlim, ylim, by, mu1, nu,scl, sig2, phi)
rp <- lr$point_lat
ra <- lr$area_lat
