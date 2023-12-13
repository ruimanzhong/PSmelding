colsc <- function(...) {
  scale_fill_gradientn(
    colors = c('viridis'),
    limits = range(..., na.rm = TRUE)
  )
}

# Project 1 ---------------------------------------------------------------
# Figure 5, Figure 6, Figure 7, Figure 8
fnMSEplot <- function(df.long){
  #p1 = ggplot(final, aes(x = Point, y= MSE, fill=Method)) + geom_boxplot() +ggtitle(title)+ xlab("Number of points")+ theme(text = element_text(size = 22)) +   theme(legend.text = element_text(size=30), legend.title = element_text(size=30))
  g <- ggplot(data = df.long) +
    geom_boxplot(aes(x = Point, y= MSE, fill=Method)) +
    facet_wrap(facets = ~Scenario, nrow = 2, ncol = 4)+ xlab("Number of points") + theme_bw() + theme(strip.text.x = element_text(size = 20), legend.position = "top", legend.direction = "horizontal", legend.text = element_text(size = 28), axis.text = element_text(size = 20), axis.title = element_text(size = 20))
  return(g)
}

fnPreplot <- function(df.long){
  #p1 = ggplot(final, aes(x = Point, y= MSE, fill=Method)) + geom_boxplot() +ggtitle(title)+ xlab("Number of points")+ theme(text = element_text(size = 22)) +   theme(legend.text = element_text(size=30), legend.title = element_text(size=30))
  g <- ggplot(data = df.long) +
    geom_boxplot(aes(x = Point, y= MSE, fill= as.factor(Prefer))) +
    facet_wrap(facets = ~Scenario, nrow = 2, ncol = 4)+ xlab("Number of points")+ theme_bw()+ theme(strip.text.x = element_text(size = 20))
 return(g)
}

# Figure 1

fnfigure1 <- function(r,depoint,dearea,dppoint, name){
  rplot <- as.data.frame(r, xy = T)
  loct <- as.data.frame(st_coordinates(depoint))
  coord <- st_coordinates(dppoint)
  # res[name][[1]][[1]]$pred_ll
  data <- as.data.frame(cbind(coord, pred_ll = dppoint$pred_ll, pred_mean = dppoint$pred_mean, pred_ul = dppoint$pred_ul))
  range = range(c(rplot$z,data$pred_ll, data$pred_mean, data$pred_ul))
  p1 <- ggplot() +
    geom_raster(data = rplot, aes(x = x, y = y, fill = z)) + coord_equal() + colsc(c(rplot$z,data$pred_ll, data$pred_mean, data$pred_ul)) + ggtitle('Target field')+ xlab('') + ylab('') + 
  p2 <- ggplot(data = boundaryregion) + geom_sf(colour = "white") + ggtitle('Point data') + geom_sf(data = depoint, aes(col = value)) + scale_colour_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "RdYlBu")), limits = range) 
  p3 <- ggplot() + geom_sf(data = dearea, aes(fill = value)) + ggtitle('Areal data') + scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "RdYlBu")), limits = range)
  
  p11 <- ggplot() + geom_raster(data = data,aes(x = X, y = Y, fill = pred_ll))+ coord_equal()  +
    colsc(c(rplot$z,data$pred_ll, data$pred_mean, data$pred_ul)) + ggtitle(paste(name, '2.5%'))+ xlab('') + ylab('')
  p22 <- ggplot() + geom_raster(data = data, aes(x = X, y = Y, fill = pred_mean)) +
    colsc(c(rplot$z,data$pred_ll, data$pred_mean, data$pred_ul))+ coord_equal()  + ggtitle(paste(name, 'Mean'))+ xlab('') + ylab('')
  p33 <- ggplot() + geom_raster(data = data, aes(x = X, y = Y, fill = pred_ul))+ coord_equal()  +
    colsc(c(rplot$z,data$pred_ll, data$pred_mean, data$pred_ul)) + ggtitle(paste(name, '97.5%')) + xlab('') + ylab('')
  
  l1 =  ggpubr::ggarrange(p1, p2,p3,  nrow = 1, ncol = 3, common.legend = TRUE)
  l2 =  ggpubr::ggarrange(p11, p22, p33,  nrow = 1, ncol = 3, legend = 'none')
  ggpubr::ggarrange(l1,l2,  nrow = 2, ncol = 1, common.legend = TRUE)
}

# Figure 10
fnfigure10 <- function(boundaryregion){ 
  crsproj <- "+proj=utm +zone=29 +nord +units=km"
  boundaryregion <- boundaryregion %>% st_transform(4326)
  bb <- unname(attributes(st_geometry(boundaryregion))$bbox)
  # Grid
  x <- seq(bb[1] - 1, bb[3] + 1, length.out = 100)
  y <- seq(bb[2] - 1, bb[4] + 1, length.out = 100)
  coop <- expand.grid(x, y)
  coop_sf <- sf::st_as_sf(coop, coords = c('Var1','Var2'), crs = st_crs(boundaryregion))
  # Keep points inside
  dpcontsurface <- coop_sf %>% st_join(boundaryregion, left = FALSE) %>% st_join(boundaryregion, left = FALSE)
  coords <- as.matrix(st_coordinates(dpcontsurface))
  s <- as.data.frame(cbind(coords, Temperature = raster::extract(temp_uk, coords) - 271))
  p <- as.data.frame(cbind(coords, Precipitation = raster::extract(precipation_uk, coords)))
  dp <- dpcontsurface %>% st_transform(crsproj)
  coords <- as.matrix(st_coordinates(dp))
  
  r <- as.data.frame(cbind(coords, Distance = raster::extract(road_raster, coords)))
  
  boundaryregion <- st_transform(boundaryregion, crsproj)
  s_sf = s %>% st_as_sf(coords = c("X", "Y"), dim = "XY")  %>% st_cast("POINT") %>% st_set_crs(4326) %>% st_transform(crsproj)
  p_sf = p %>% st_as_sf(coords = c("X", "Y"), dim = "XY")  %>% st_cast("POINT") %>% st_set_crs(4326) %>% st_transform(crsproj)
  r_sf = r %>% st_as_sf(coords = c("X", "Y"), dim = "XY")  %>% st_cast("POINT") %>% st_set_crs(crsproj)
  p11 <- ggplot(data = boundaryregion) + geom_sf() +geom_sf(data = s_sf, aes(color = Temperature)) + theme_minimal() + ggtitle('Temperature (Celsius)')
  p12 <- ggplot(data = boundaryregion) + geom_sf() +geom_sf(data = p_sf, aes(color = Precipitation)) + theme_minimal() + ggtitle('Precipitation (m)')
  p13 <- ggplot(data = boundaryregion) + geom_sf() +geom_sf(data = r_sf, aes(color = Distance)) + theme_minimal() + ggtitle('Distance (km)')
  options(ggplot2.continuous.colour="viridis")
  options(ggplot2.continuous.fill = "viridis")
  ggpubr::ggarrange(p11,p12, p13, nrow = 1) }
  

# Project 2 ---------------------------------------------------------------

# Generate plot for both area data and point level data

figure_misdata <- function(darea,depoint, title, unit){
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

plot_sp_3 <- function(results, range){
  attach(results)
  coop <- st_coordinates(results)
  dpm <- rbind(
    data.frame(
      X = coop[, 1], Y = coop[, 2],
      Prob = ecprob_PSgeo, variable = "PS Geo"
    ),
    data.frame(
      X = coop[, 1], Y = coop[, 2],
      Prob = ecprob_PSmeld, variable = "PSmelding"
    ),
    data.frame(
      X = coop[, 1], Y = coop[, 2],
      Prob = ecprob_meld, variable = "Melding"
    )
  )
  dpm$variable <- as.factor(dpm$variable)
  
  p = ggplot(dpm, aes(X,Y,fill = Prob)) +
    geom_tile(size = 1)+
    facet_grid(~ variable)+ scale_fill_viridis_c(limits = range)+
    coord_fixed(ratio = 1)+
    geom_sf(data = boundaryregion_plot, inherit.aes = F, fill = NA) +  labs(x = "", y = "") +
    theme(title = element_text(size = 18),strip.text.x = element_text(size = 18), axis.title = element_text(size = 18)) +
    ggtitle('exceedance probabilities')
  
  png(paste0( 'excrop_Predict_res','.png'), width = 1280, height = 700)
  print(p)
  dev.off()
  return(p)
}

createLatexTable <- function(t1,t2,t3){
  if(class(t2) != c("data.frame")){  
    table <- left_join(t1,t3,by = c("Areas", "Points"), suffix = c(" PSmelding ", " PSgeo") ) } else {  table <- left_join(t1,t2,by = c("Areas", "Points"), suffix = c(" PSmelding ", " Melding") ) %>% 
      left_join(t3, by = c("Areas", "Points"), suffix = c(" ", " PSgeo"))}
  
  table$Areas <- as.factor(table$Areas)
  table$Points <- as.factor(table$Points)
  table <- table%>% 
    mutate_if(is.numeric, round, digits = 2) 
  return(table)
}
