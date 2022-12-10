###################################################
# Study region, mesh and prediction points
###################################################

# Study region
xlim <- c(0,1)
ylim <- c(0,1)
by <- 0.02

# Objects to fit the model

# Boundary region
win <- owin(xrange = xlim, yrange = ylim)
boundaryregion <- sf::st_as_sf(win)

# Mesh
loc.d <- cbind(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0))
mesh <- inla.mesh.2d(loc.domain = loc.d, offset = c(0.1, 0.35), max.edge = c(0.1, 0.25), cutoff = 0.01)
plot(mesh)

# dppoint (prediction points)
x <- seq(from = xlim[1] + (by / 2), to = xlim[2] - (by / 2), by = by)
y <- seq(from = ylim[1] + (by / 2), to = ylim[2] - (by / 2), by = by)
coord <- expand.grid(x = x, y = y)
coop_sf <- sf::st_as_sf(coord, coords = c('x','y'))
dppoint <- coop_sf %>% st_join(boundaryregion, left = FALSE)




###################################################
# Main functions
###################################################

fnGenerateSurface <- function(xlim, ylim, by, mu1, mu0, nu, nu0, scl, scl0, sig2, sig20){
  # Generate surface
  r <- latt_generation(xlim, ylim, by, mu1, nu, scl, sig2, seed = 123)
  # Generate surface to sample points
  rs <- latt_generation(xlim, ylim, by, mu0, nu0, scl0, sig20, seed = 123)
  return(list(r, rs))
}


fnMeasurementsatPointsAndAreas <- function(pnumm, anumm, r, rs){
  # Generating locations at which taking observations using Preferential sampling based on rs 
  loct <- pcoxsample(pnumm, rs)
  # Point data
  p3 <- datagenerator(loct, r) 
  depoint <- p3 %>% st_as_sf(coords = c("x", "y"), dim = "XY") %>% st_cast("POINT")
  # Areal data
  dearea <- areasample(anumm, r) %>% st_make_valid()%>% st_set_crs(NA_crs_)
  return(list(depoint, dearea))
}



fnFitModels <- function(depoint, dearea = NULL, dppoint = NULL , dparea = NULL,
                        boundaryregion, mesh = NULL, prior.sigma = NULL, prior.range = NULL, loc.d){
  
  # output return(list(dp1, dp2, res, spde))
  
  res_points <- fnPredictMelding(depoint = depoint, dearea = NULL, dppoint = dppoint, dparea = NULL,
                                 boundaryregion = boundaryregion, mesh = mesh, prior.sigma, prior.range)
  
  resPS_points <- fnPredictMeldingPS(depoint = depoint, dearea = NULL, dppoint = dppoint, dparea = NULL,
                                     boundaryregion = boundaryregion, mesh = mesh, prior.sigma, prior.range, loc.d = loc.d)
  
  res_pointsareas <- fnPredictMelding(depoint = depoint, dearea = dearea, dppoint = dppoint, dparea = NULL,
                                      boundaryregion = boundaryregion, mesh = mesh, prior.sigma, prior.range)
  
  resPS_pointsareas <- fnPredictMeldingPS(depoint = depoint, dearea = dearea, dppoint = dppoint, dparea = NULL,
                                          boundaryregion = boundaryregion, mesh = mesh, prior.sigma, prior.range, loc.d = loc.d)
  
  res <- makeNamedList(res_points, resPS_points, res_pointsareas, resPS_pointsareas)
  names <- names(res)
  
  return(res)
}