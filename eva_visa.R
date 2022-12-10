colsc <- function(...) {
  scale_fill_gradientn(
    colours = rev(RColorBrewer::brewer.pal(11, "RdYlBu")),
    limits = range(..., na.rm = TRUE)
  )
}

latent_plot <- function(r, rs, depoint, dearea){
#par(mfrow = c(1, 2))
rplot <- as.data.frame(r, xy = T)
rsplot <-as.data.frame(rs, xy = T) 
loct <- as.data.frame(st_coordinates(depoint))

p1 <- ggplot() +
  geom_raster(data = rplot, aes(x = x, y = y, fill = z)) + colsc(rplot$z) +
  geom_point(data = loct, aes(x = loct[, 1], y = loct[,2]), col = 1) + ggtitle('Obs on Target field')

p2 <- ggplot() +
  geom_raster(data = rsplot, aes(x = x, y = y, fill = z)) + colsc(rsplot$z) +
  geom_point(data = loct, aes(x = loct[, 1], y = loct[,2]), col = 1) + ggtitle('Obs on Generating field')
 
p3 <- ggplot(data = boundaryregion) + geom_sf() + ggtitle('Point data') +
  geom_sf(data = depoint, aes(col = value)) +
  scale_colour_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "RdYlBu")), limits = range(rplot$z, na.rm = TRUE))

p4 <- NULL
if(!is.null(dearea)){
p4 <- ggplot() + geom_sf(data = dearea, aes(fill = value)) + ggtitle('Areal data') +
    scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "RdYlBu")), limits = range(rplot$z, na.rm = TRUE))
}

ggpubr::ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2, common.legend = TRUE)
} 


  
raster_plot <- function(name, dppoint, ...){
coord <- st_coordinates(dppoint)
data <- as.data.frame(cbind(coord, prediction = res[name][[1]][[1]]$pred_ll,
                            pred_mean = res[name][[1]][[1]]$pred_mean,
                            pred_ul = res[name][[1]][[1]]$pred_ul))
  
p1 <- ggplot() + geom_raster(data = data,aes(x = X, y = Y, fill = prediction)) +
  colsc(c(data$prediction, data$pred_mean, data$pred_ul)) + ggtitle(paste(name,'2.5% quantile'))
p2 <- ggplot() + geom_raster(data = data, aes(x = X, y = Y, fill = pred_mean)) +
  colsc(c(data$prediction, data$pred_mean, data$pred_ul)) + ggtitle(paste(name,'Posterior mean'))
p3 <- ggplot() + geom_raster(data = data, aes(x = X, y = Y, fill = pred_ul)) +
  colsc(c(data$prediction, data$pred_mean, data$pred_ul)) + ggtitle(paste(name,'97.5% quantile'))

ggpubr::ggarrange(p1, p2, p3, ncol = 3, common.legend = TRUE)
}

#######################

fnPlotLatentAndPrediction <- function(r, rs, depoint, dearea, res, name, dppoint){
  #par(mfrow = c(1, 2))
  rplot <- as.data.frame(r, xy = T)
  rsplot <-as.data.frame(rs, xy = T) 
  loct <- as.data.frame(st_coordinates(depoint))
  
  p1 <- ggplot() +
    geom_raster(data = rplot, aes(x = x, y = y, fill = z)) + colsc(rplot$z) +
    geom_point(data = loct, aes(x = loct[, 1], y = loct[,2]), col = 1) + ggtitle('Target field')
  
  p2 <- ggplot() +
    geom_raster(data = rsplot, aes(x = x, y = y, fill = z)) + colsc(rsplot$z) +
    geom_point(data = loct, aes(x = loct[, 1], y = loct[,2]), col = 1) + ggtitle('Generating field')
  
  p3 <- ggplot(data = boundaryregion) + geom_sf() + ggtitle('Point data') +
    geom_sf(data = depoint, aes(col = value)) +
    scale_colour_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "RdYlBu")), limits = range(rplot$z, na.rm = TRUE))
  
  
  p4 <- NULL
  if(!is.null(dearea)){
    p4 <- ggplot() + geom_sf(data = dearea, aes(fill = value)) + ggtitle('Areal data') +
      scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "RdYlBu")), limits = range(rplot$z, na.rm = TRUE))
  }
  
  #ggpubr::ggarrange(p1, p2, p3, ncol = 3, common.legend = TRUE)

  coord <- st_coordinates(dppoint)
  # res[name][[1]][[1]]$pred_ll
  data <- as.data.frame(cbind(coord, pred_ll = dppoint$pred_ll, pred_mean = dppoint$pred_mean, pred_ul = dppoint$pred_ul))
  
  p11 <- ggplot() + geom_raster(data = data,aes(x = X, y = Y, fill = pred_ll)) +
    colsc(c(data$pred_ll, data$pred_mean, data$pred_ul)) + ggtitle(paste(name, '2.5%'))
  p22 <- ggplot() + geom_raster(data = data, aes(x = X, y = Y, fill = pred_mean)) +
    colsc(c(data$pred_ll, data$pred_mean, data$pred_ul)) + ggtitle(paste(name, 'Mean'))
  p33 <- ggplot() + geom_raster(data = data, aes(x = X, y = Y, fill = pred_ul)) +
    colsc(c(data$pred_ll, data$pred_mean, data$pred_ul)) + ggtitle(paste(name, '97.5%'))
  
  ggpubr::ggarrange(p1, p2, p3, p4, p11, p22, p33, nrow = 2, ncol = 4, common.legend = TRUE)
}

###############################
  

fnEvaluation <- function(res, truesurface_sf, pnum, anum) {
len <- length(res)
names <- names(res)

fnError <- function(name) {
errorm <-  truesurface_sf %>% st_join(res[name][[1]][[1]]) %>% mutate(error = pred_mean - true) %>% dplyr::select(error, geometry)
p <- ggplot(data = errorm) + geom_sf(aes(col = error)) +
  scale_colour_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "RdYlBu"))) +
  ggtitle(paste(name,beta0))+ xlab(paste("Pnum", pnum, 'Anum', anum^2))
ll <- list(errorm, p)
names(ll) <- c(paste(name, 'error'), paste(name, 'plot'))
return(ll)
}
  
eva <-  lapply(names, fnError)
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
