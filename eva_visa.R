colsc <- function(...) {
  scale_fill_gradientn(
    colours = rev(RColorBrewer::brewer.pal(11, "RdYlBu")),
    limits = range(..., na.rm = TRUE)
  )
}

latent_plot <- function(r, rs, depoint){
  par(mfrow = c(1,2))
  rplot <- as.data.frame(r, xy = T)
  rsplot <-as.data.frame(rs, xy = T) 
  loct <- as.data.frame(st_coordinates(depoint))
 p1 = ggplot() +
    geom_raster(data = rplot,
                aes(x = x, y = y, fill = z)) + colsc(rplot$z) +
     geom_point(data = loct, aes(x = loct[, 1], y = loct[,2]), col = 1) + ggtitle('Observation on Target field')

 p2  = ggplot() +
   geom_raster(data = rsplot,
               aes(x = x, y = y, fill = z)) + colsc(rsplot$z) +
   geom_point(data = loct, aes(x = loct[, 1], y = loct[,2]), col = 1) + ggtitle('Observation on Generating field')
 
 p3 = ggplot(data = boundaryregion) + geom_sf()  + ggtitle('Observations on Target field') +
   geom_sf(data = depoint, aes(col = value)) + scale_colour_gradientn(
     colours = rev(RColorBrewer::brewer.pal(11, "RdYlBu")),
     limits = range(rplot$z, na.rm = TRUE)
   )
 ggpubr::ggarrange(p1,p2,p3, ncol = 3)
} 


  
raster_plot <- function(name, dppoint, ...){
  coord <- st_coordinates(dppoint)
  data <- as.data.frame(cbind(coord, prediction = res[name][[1]][[1]]$pred_ll, pred_mean = res[name][[1]][[1]]$pred_mean, pred_ul = res[name][[1]][[1]]$pred_ul))
  
  p1 = ggplot() +
    geom_raster(data = data,aes(x = X, y = Y, fill = prediction)) + colsc(c(data$prediction, data$pred_mean, data$pred_ul)) + ggtitle(paste(name,'2.5% quantile Prediction'))
  p2 = ggplot() +
    geom_raster(data = data,
                aes(x = X, y = Y, fill = pred_mean)) + colsc(c(data$prediction, data$pred_mean, data$pred_ul)) + ggtitle(paste(name,'Mean Prediction'))
  p3 = ggplot() +
    geom_raster(data = data,
                aes(x = X, y = Y, fill = pred_ul)) + colsc(c(data$prediction, data$pred_mean, data$pred_ul)) + ggtitle(paste(name,'97.5% quantile Prediction'))
  
  ggpubr::ggarrange(p1,p2,p3, ncol = 3, common.legend = T)
}
  

fnEvaluation <- function(res,truesurface_sf, pnum, anum) {
  len <- length(res)
  names <- names(res)
  error <- function(name) {
    e =  truesurface_sf %>% st_join(res[name][[1]][[1]]) %>% mutate(error = pred_mean - true) %>% dplyr::select(error, geometry)
    p <-ggplot(data = e) + geom_sf(aes(col = error)) + scale_colour_gradientn(
      colours = rev(RColorBrewer::brewer.pal(11, "RdYlBu")))  + ggtitle(paste(name,beta0))+ xlab(paste("Pnum", pnum, 'Anum', anum^2))
    list = list(e, p)
    names(list) = c(paste(name,'error'), paste(name,'plot'))
    return(list)
  }
  
  eva =  lapply(names, error)
  names(eva) <- names
  
  return(eva)
  
}

makeNamedList <- function(...) {
  structure(list(...), names = as.list(substitute(list(...)))[-1L])
}

myMSE <- function(l){
  name <- names(l)
  e <- l[[name]][[paste(name,'error')]][["error"]]
  return(list(mean(e^2), mean(abs(e))))
}

 
fnPosLatentplot<- function(res, spde){
  par(mfrow = c(3,1))
  plot(res$marginals.fix[[1]], type = "l", main="Posterior density for Beta0.")
  result.field = inla.spde.result(res, "s", spde, do.transform = T)
  plot(result.field$marginals.range.nominal[[1]],
       type="l", main="Posterior density for range")
  plot(inla.tmarginal(sqrt, result.field$marginals.variance.nominal[[1]]),
       type="l", main="Posterior density for std.dev.")
}
