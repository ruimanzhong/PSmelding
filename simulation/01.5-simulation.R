#Parameters
nu = 1
mu1 <- param$mu1
mu0 <- param$mu0
nu0 <- param$nu
scl <- param$scl
scl0 <- param$scl0
sig2 <- param$sig2
sig20 <- param$sig20
sig.err <- param$sig.err
beta1 <- param$beta1
beta0 <- param$beta0
prior.sigma <- prior.sigma
prior.range <- prior.range
print(is.null(index))
# Generate surface

print("Generate surface")
lr <- fnGenerateSurface(xlim, ylim, by, mu1, mu0, nu, nu0, scl, scl0, sig2, sig20)
r <- lr[1][[1]]
rs <- r