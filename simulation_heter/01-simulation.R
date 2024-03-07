###################################################
# Study region, mesh and prediction points
###################################################

# Study region
xlim <- c(0,1)
ylim <- c(0,1)
by <- 0.02
by = 0.02
# Objects to fit the model

# Boundary region
win <- owin(xrange = xlim, yrange = ylim)
boundaryregion <- sf::st_as_sf(win)

# Mesh
loc.d <- cbind(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0))
mesh <- inla.mesh.2d(loc.domain = loc.d, offset = c(0.1, 0.35), max.edge = c(0.05, 0.25), cutoff = 0.01)
#plot(mesh)

# dppoint (prediction points)
x <- seq(from = xlim[1] + (by / 2), to = xlim[2] - (by / 2), by = by)
y <- seq(from = ylim[1] + (by / 2), to = ylim[2] - (by / 2), by = by)
coord <- expand.grid(x = x, y = y)
coop_sf <- sf::st_as_sf(coord, coords = c('x','y'))
dppoint <- coop_sf %>% st_join(boundaryregion, left = FALSE)


###################################################
# Main functions
###################################################

fnGenerateSurface_heter <- function(xlim, ylim, by, mu1, nu,scl,sig2, phi){
  # Generate surface
  r <- latt_generation_heter(xlim, ylim, by, mu1, nu, scl, sig2, phi)
  # Generate surface to sample points
  rp <- r$point_lat
  ra <- r$area_lat
  return(list(point_lat = rp, area_lat = ra))
}



fnMeasurementsatPointsAndAreas_heter <- function(pnumm, anumm, ra, rp, seed, beta1, beta0, sig.err){
  # Generating locations at which taking observations using Preferential sampling based on rs 
  loct <- pcoxsample(pnumm,rp,beta0, beta1)
  # Point data
  p3 <- datagenerator(loct, rp, sig.err) 
  depoint <- p3 %>% st_as_sf(coords = c("x", "y"), dim = "XY") %>% st_cast("POINT")
  # Areal data
  dearea <- areasample(anumm, ra, sig.err) %>% st_make_valid()%>% st_set_crs(NA_crs_)
  return(list(depoint, dearea))
}



fnFitModels <- function(depoint, dearea = NULL, dppoint = NULL , dparea = NULL,
                        boundaryregion, mesh = NULL, prior.sigma = NULL, prior.range = NULL, loc.d){
  # tic.clearlog()
  # tic('Geostat')
  # Geostat <- fnPredictMelding(depoint = depoint, dearea = NULL, dppoint = dppoint, dparea = NULL,
  #                                boundaryregion = boundaryregion, mesh = mesh, prior.sigma, prior.range)
  # toc(log = T)
  
  tic('PS_geo')
  PS_geo <- fnPredictMeldingPS(depoint = depoint, dearea = NULL, dppoint = dppoint, dparea = NULL,
                                     boundaryregion = boundaryregion, mesh = mesh, prior.sigma, prior.range, loc.d = loc.d)
  toc(log = T)
  
  tic('Melding')
  Melding <- fnPSMeldingCOVTwo(depoint = depoint, dearea = dearea, dppoint = dppoint, dparea = NULL,
                               boundaryregion = boundaryregion, mesh = mesh, 
                               prior.sigma, prior.range, covariate = NULL, PS = F,q= NULL)
  toc(log = T)
  
  tic('PS_melding')
  PS_melding <- fnPSMeldingCOVTwo(depoint = depoint, dearea = dearea, dppoint = dppoint, dparea = NULL,
                                  boundaryregion = boundaryregion, mesh = mesh, 
                                  prior.sigma, prior.range, covariate = NULL, PS = T,q= NULL)
  toc(log = T)
  time = tic.log(format = T)
  res <- makeNamedList(PS_geo, Melding, PS_melding, time)
  names <- names(res)
  return(res)
}
