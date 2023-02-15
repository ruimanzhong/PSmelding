
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
  
# 
# fnEvaluation <- function(res, truesurface_sf, pnum, anum, ...) {
# # if(!"Cluster" %in% names(truesurface_sf)) {truesurface_sf$Cluster <- rep(1, nrow(truesurface_sf)) }
# len <- length(res)-1
# names <- names(res)[1:len]
# 
# e <- lapply(names, function(name) res[[name]][[1]][["pred_mean"]] - truesurface_sf$true)
# names(e) <- names
# ME <- lapply(names, function(name) return(list (MSE = mean(e[[name]]^2), MAE = mean(abs(e[[name]]), time  = res[['time']][[name]]))))
# names(ME) <- names
# ME <- unlist(ME)
# return(ME)
#   
# }
# 
# makeNamedList <- function(...) {
#   structure(list(...), names = as.list(substitute(list(...)))[-1L])
# }

 
fnPosLatentplot<- function(res, spde){
  par(mfrow = c(3,1))
  plot(res$marginals.fix[[1]], type = "l", main="Posterior density for Beta0.")
  result.field = inla.spde.result(res, "s", spde, do.transform = T)
  plot(result.field$marginals.range.nominal[[1]],
       type="l", main="Posterior density for range")
  plot(inla.tmarginal(sqrt, result.field$marginals.variance.nominal[[1]]),
       type="l", main="Posterior density for std.dev.")
}
# 
# myerrorD <- function(P,Py, y, name){
#   wd = wasserstein1d(P, y, p = 1, wa = NULL, wb = NULL)
#   crps_py = scoringutils::crps_sample(y, t(P))
#   crps_pp = scoringutils::crps_sample(Py, t(P))
#   scrps <- -0.5*(1+ crps_py/crps_pp + log(2* abs(crps_pp)))
#   metric <- list(scrps, wd)
#   names(metric) <- c(paste(name, 'scrps'), paste(name, 'WD'))
#   return(metric)
# }
fnmodelconstructplot <- function(depoint,dearea = NULL,r, res, name, dppoint){
  rplot <- as.data.frame(r, xy = T)
  rsplot <-as.data.frame(rs, xy = T) 
  loct <- as.data.frame(st_coordinates(depoint))
  
  p1 <- ggplot(data = boundaryregion) + geom_sf() + ggtitle('Point data') +
    geom_sf(data = depoint, aes(col = value)) +
    scale_colour_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "RdYlBu")), limits = range(c(data$pred_ll, data$pred_mean, data$pred_ul), na.rm = TRUE))
  
  p2 <- NULL
  if(!is.null(dearea)){
    p2 <- ggplot() + geom_sf(data = dearea, aes(fill = value)) + ggtitle('Areal data') +
      scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "RdYlBu")), limits = range(c(data$pred_ll, data$pred_mean, data$pred_ul), na.rm = TRUE))
  }
  
  p11 <- ggplot() + geom_raster(data = data,aes(x = X, y = Y, fill = pred_ll)) +
    colsc(c(data$pred_ll, data$pred_mean, data$pred_ul)) + ggtitle(paste(name, '2.5%'))+ labs(x = "", y = '')
  p22 <- ggplot() + geom_raster(data = data, aes(x = X, y = Y, fill = pred_mean)) +
    colsc(c(data$pred_ll, data$pred_mean, data$pred_ul)) + ggtitle(paste(name, 'Mean'))+ labs(x = "", y = '')
  p33 <- ggplot() + geom_raster(data = data, aes(x = X, y = Y, fill = pred_ul)) +
    colsc(c(data$pred_ll, data$pred_mean, data$pred_ul)) + ggtitle(paste(name, '97.5%'))+ labs(x = "", y = '')
  
 l1 =  ggpubr::ggarrange(p1, p2,  nrow = 1, ncol = 2, legend = 'none')
 l2 =  ggpubr::ggarrange(p11, p22, p33,  nrow = 1, ncol = 3,common.legend = TRUE, legend = 'right')
  ggpubr::ggarrange(l1,l2,  nrow = 2, ncol = 1, common.legend = TRUE)
 
}

fnboxplot <- function(final,range,title, ..){
  
  p1 = ggplot(final, aes(x = Point, y= MSE, fill=Method)) +
    geom_boxplot() +ggtitle('Scenario 1 var = 1 scale = 0.01')+ xlab("Number of points")+ theme(text = element_text(size = 22)) +   theme(legend.text = element_text(size=30), legend.title = element_text(size=30))
  
}

