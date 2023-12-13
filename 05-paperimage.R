source('eva_visa.R')

# nu = 1
# scl = 0.05
# scl0 = 0.05
# mu1 = 0
# mu0 = 0
# sig.err = 0.1*4
# sig2 = 4
# sig20 = 4
# beta1 = 1
# beta0 = 0.5
# pnumm = 250
# anumm = 5
# prior.range = c(0.05, 0.1)
# prior.sigma = c(1, 0.9)
# 
# lr <- fnGenerateSurface(xlim, ylim, by, mu1, mu0, nu, nu0, scl, scl0, sig2, sig20)
# r <- lr[1][[1]]
# rs <- r
# 
# 
# print("Take measurements at points and areas")
# # Take measurements at points and areas
# lpa <- fnMeasurementsatPointsAndAreas(pnumm, anumm, r, rs, seed, beta1 = beta1, beta0 = beta0, sig.err = sig.err)
# depoint <- lpa[1][[1]]
# dearea <- lpa[2][[1]]
# 
# png(paste0('Simu_field','.png'), width = 1280, height = 700)
# fnfigure1(r,depoint,dearea)
# dev.off()

fnfigure1 <- function(r,depoint, dearea){
  rplot <- as.data.frame(r, xy = T)
  loct <- as.data.frame(st_coordinates(depoint))
  coord <- st_coordinates(depoint)
  range = range(c(rplot$z,depoint$value))
  p1 <- ggplot() +
    geom_raster(data = rplot, aes(x = x, y = y, fill = z)) + coord_equal() + geom_point(aes(coord[,1], coord[,2]),shape = 2 ) + colsc(range) + 
    ggtitle('True field')+ xlab('') + ylab('') + theme_bw()+ theme( title = element_text(size = 20))
  p2 <- ggplot(data = boundaryregion) + geom_sf(colour = "white") + ggtitle('Point data') +
    geom_sf(data = depoint, aes(col = value)) +
    scale_colour_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "RdYlBu")), limits = range) + 
    theme_bw()+ theme( title = element_text(size = 20))
  p3 <- ggplot() + geom_sf(data = dearea, aes(fill = value)) + ggtitle('Areal data') +
    scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "RdYlBu")), limits = range) +
   theme_bw()+ theme( title = element_text(size = 20))
  ggpubr::ggarrange(p1, p2, p3, nrow = 1, ncol = 3, common.legend = TRUE,legend = "right")
}

fnFigure_box <- function(df.long, points, pc = T){
  df.long$Model <- as.factor(df.long$Model)
  # levels(df.long$Model) <- c("Geostat", "Melding", 'PSgeo', 'PSmelding')
  #p1 = ggplot(final, aes(x = Point, y= MSE, fill=Method)) + geom_boxplot() +ggtitle(title)+ xlab("Number of points")+ theme(text = element_text(size = 22)) +   theme(legend.text = element_text(size=30), legend.title = element_text(size=30))
  g1 <- ggplot(data = df.long) + geom_boxplot(aes(x = as.factor(theta), y= MSE , fill= Model )) + 
    facet_wrap(facets = ~Points, nrow = 1, ncol = 2,labeller = "label_both")+ xlab("MSE") + theme_bw() + theme(strip.text.x = element_text(size = 20), legend.position = "top", legend.direction = "horizontal", legend.text = element_text(size = 20), axis.text = element_text(size = 20), axis.title = element_text(size = 20))
  g2 <- ggplot(data = df.long) + geom_boxplot(aes_string(x = 'as.factor(range)', y= type, fill= 'Model' ))  +  
    # scale_y_continuous(limits = quantile(d2[[y]], c(0.005, 0.995))) + 
    facet_wrap(facets = ~Points, nrow = 1, ncol = 2,labeller = "label_both")+ xlab("MAE") + theme_bw() + theme(strip.text.x = element_text(size = 20), legend.position = "top", legend.direction = "horizontal", legend.text = element_text(size = 20), axis.text = element_text(size = 20), axis.title = element_text(size = 20))
  g3 <- ggplot(data = df.long) + geom_boxplot(aes_string(x = 'as.factor(range)', y= type, fill= 'Model' ))  +  
    # scale_y_continuous(limits = quantile(d2[[y]], c(0.005, 0.995))) + 
    facet_wrap(facets = ~Points, nrow = 1, ncol = 2,labeller = "label_both")+ xlab("WD") + theme_bw() + theme(strip.text.x = element_text(size = 20), legend.position = "top", legend.direction = "horizontal", legend.text = element_text(size = 20), axis.text = element_text(size = 20), axis.title = element_text(size = 20))
  
  if(pc){png(paste0(type, 'pc',points,'.png'), width = 1280, height = 700)}
  if(!pc){png(paste0(type, 'default',points,'.png'), width = 1280, height = 700)}
  print(ggpubr::ggarrange(g1,g2, g3, nrow = 1))
  dev.off()
}

figure_misdata <- function(darea,depoint,boundaryregion_plot, title, unit){
  p1 <- ggplot(data = boundaryregion_plot) + geom_sf() +
    geom_sf(data = depoint, aes(col = value), size = 2)+
    scale_fill_viridis_c(limits = range(dearea$value,depoint$value)) +
    theme(strip.text.x = element_text(size = 18), 
          legend.position = "top", 
          legend.direction = "horizontal", 
          legend.text = element_text(size = 11), 
          # axis.text = element_text(size = 16),
          legend.title = element_text(size = 18), 
          plot.title = element_text(size = 18))
  
  p2 <- ggplot(data = boundaryregion_plot) + geom_sf() +
    geom_sf(data = dearea, aes(fill = value)) +
    scale_fill_viridis_c(limits = range(dearea$value,depoint$value)) + 
    theme(strip.text.x = element_text(size = 18), 
          legend.position = "top", 
          legend.direction = "horizontal", 
          legend.text = element_text(size = 11), 
          # axis.text = element_text(size = 16),
          legend.title = element_text(size = 18), 
          plot.title = element_text(size = 18))
  p <- ggpubr::ggarrange(p1,p2, col = 2 , common.legend =T, legend = 'top')
  png(paste0('Area_data','.png'), width = 1280, height = 700)
  print(p)
  dev.off()
  return(p)
}

plot_sp_2 <- function(results, model, range,boundaryregion_plot){
  coop <- st_coordinates(results)
  pred_mean = results$pred_mean
  pred_ll = results$pred_ll
  pred_ul = results$pred_ul
  dpm <- rbind(
    data.frame(
      X = coop[, 1], Y = coop[, 2],
      PM2.5 = pred_mean, variable = "Mean"
    ),
    data.frame(
      X = coop[, 1], Y = coop[, 2],
      PM2.5 = pred_ll, variable = "Lower bound 2.5% "
    ),
    data.frame(
      X = coop[, 1], Y = coop[, 2],
      PM2.5 = pred_ul, variable = "Upper bound 97.5%"
    )
  )
  dpm$variable <- as.factor(dpm$variable)
  
  p = ggplot(dpm, aes(X,Y,fill = PM2.5)) +
    geom_tile(size = 1)+
    facet_grid(~ variable)+ scale_fill_viridis_c(limits = range)+
    coord_fixed(ratio = 1)+
    geom_sf(data = boundaryregion_plot, inherit.aes = F, fill = NA) +  labs(x = "", y = "") +
    theme(title = element_text(size = 18),strip.text.x = element_text(size = 18), axis.title = element_text(size = 18)) +
    ggtitle(paste('Bayesian',model))
  
  png(paste0(model, 'Predict_res','.png'), width = 1280, height = 700)
  print(p)
  dev.off()
  return(p)
}