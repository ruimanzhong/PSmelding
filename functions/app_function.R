fnTifdata <- function(str, boundaryregion, fa){
  glo_pm <- raster(str)
  rr <- mask(crop(glo_pm, boundaryregion, snap = 'out'), boundaryregion)
  ra <- raster::aggregate(rr, fact = fa, fun = mean, na.rm = T)
  return(ra)
}
fnCreatemeshcov <- function(mesh,covaraite, crsproj){
  sf_points <- st_as_sf(data.frame(X = mesh$loc[,1], Y = mesh$loc[,2]), coords = c("X", "Y"), crs = crsproj, agr = "constant")
  mesh_cov <- sf_points %>% st_join(covaraite)
  return(mesh_cov)
}