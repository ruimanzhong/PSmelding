# The preprocessed data can be found in data folder
# The first three sections are data preprocessing procedure, if you load the data, please run Modeling section directly

source("header.R")
source('05-paperimage.R')
source('dataprocess.r')
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2)
library(sp)
library(tibbletime)
library(lubridate)
library(dplyr)
library(rnaturalearth)
library(sf)
theme_set(theme_minimal())
options(ggplot2.continuous.colour="viridis")
options(ggplot2.continuous.fill = "viridis")
# boundaryregion ----------------------------------------------------------

# 
lon <- c(-124, -67)
lat <- c(20, 50)

theme_set(theme_minimal())
crsproj <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=km +nadgrids=@null +wktext +no_defs"

world <- ne_countries(scale='medium',returnclass = 'sf')
usa <- subset(world, admin == "United States of America")

boundaryregion <- usa%>%st_crop(usa, xmin =min(lon), xmax = max(lon), ymin = min(lat), ymax = max(lat))%>%st_transform(crsproj)
ggplot(boundaryregion)+ geom_sf()
loc.d <- st_coordinates(boundaryregion)[,c(1,2)]

# Fitting INLA model multiprocess------------------------------------------------------
dearea <- st_read("data/dearea_cov.shp") %>% st_join(boundaryregion,left = F) %>% dplyr::select(1,3)
depoint <- st_read("data/depoint_cov.shp")%>% st_join(boundaryregion,left = F) %>% dplyr::select(1,2)
dppoint <- st_read("data/dppoint_cov.shp")%>% st_join(boundaryregion,left = F) %>% dplyr::select(1)
population_sf <- st_read("data/county_population.shp")%>% st_join(boundaryregion,left = F) %>% dplyr::select(1)

# dppoint <- na.omit(dppoint)

boundaryregion_sp <- as_Spatial(boundaryregion[,1])
maxedge <- fnMaxEdgeMeshFromBoundary(boundaryregion)
mesh <- inla.mesh.2d(loc.domain = loc.d,boundary = boundaryregion_sp, 
                     max.edge = c(maxedge/20, maxedge), cutoff = maxedge/30
                     )
plot(mesh) 

# Modeling shared components ----------------------------------------------------------------

# pre_model <- fnPredictMelding(depoint = NULL, dearea = dearea, dppoint = dppoint, dparea = NULL,
#                               boundaryregion = boundaryregion, mesh = mesh)
# pre_model[[3]][["summary.hyperpar"]]
# kappa <- exp(-3)
# rho <- sqrt(8*1)/kappa
# sigma <- 0.1
colnames(depoint) <- c('value','population_density','geometry')
colnames(dearea) <- c('value','population_density','geometry')
colnames(dppoint) <- c('population_density','geometry')
colnames(population_sf) <- c('population_density','geometry')
# Consider_informative_cov ------------------------------------------------

mesh_cov <- fnCreatemeshcov(mesh,population_sf,crsproj)
colnames(mesh_cov) <- c('population_density','geometry')
mesh_cov <- mesh_cov %>%
  mutate(population_density = ifelse(is.na(population_density), 0, population_density))
ggplot(data = boundaryregion) + geom_sf() + geom_sf(data = mesh_cov, aes(col = population_density), size = 2)
# merge mesh node and observation node
covariate <- data.frame(rbind(mesh_cov[,c(1)],depoint[,c(2)])) %>% st_as_sf()
# Modeling ----------------------------------------------------------------
kappa <- exp(-4)
rho <- sqrt(8*1)/kappa
kappa <- exp(-1)
rho <- sqrt(8*1)/kappa
# 
 # depoint$population_density <- log(depoint$population_density +1)
 # covariate$population_density <- log(covariate$population_density +1)
 # dearea$population_density <- log(dearea$population_density +1)
 # dppoint$population_density <- log(dppoint$population_density +1)
PSmeldingPD <- fnPSMeldingCOVTwo(depoint = depoint, dearea = dearea, dppoint = dppoint, dparea = NULL,
                                 boundaryregion = boundaryregion, mesh = mesh, 
                                 prior.sigma = c(0.1,0.6), prior.range = c(rho, 0.9), covariate = covariate, PS = T,q= 8)
summary(PSmeldingPD[[2]])

xtable(PSmeldingPD[[2]]$summary.fixed[,c(1,2,3,5)])
xtable(PSmeldingPD[[2]]$summary.hyperpar[,c(1,2,3,5)])
#save(PSmeldingPD, file = 'US_pop_COV_psmeldings2_spde_beta_2019.Rdata')
plot_sp_2(PSmeldingPD[[1]],model = 'PSmelding',range(PSmeldingPD[[1]]$pred_ll,PSmeldingPD[[1]]$pred_ul),boundaryregion)

# MeldingPD <- fnPSMeldingCOVTwo(depoint = depoint, dearea = dearea, dppoint = dppoint, dparea = NULL,
#                                boundaryregion = boundaryregion, mesh = mesh, 
#                                prior.sigma = c(0.1,0.1), prior.range = c(rho, 0.9), covariate = covariate, PS = F,q=8)
# summary(MeldingPD[[2]])
 plot_sp_2(MeldingPD[[1]],model = 'Melding',range(MeldingPD[[1]]$pred_ll,MeldingPD[[1]]$pred_ul),boundaryregion)
 # save(MeldingPD, file = 'US_pop_COV_melding_spde_2019.Rdata')
# PSgeoPD <- fnPSMeldingCOVTwo(depoint = depoint, dearea = NULL, dppoint = dppoint, dparea = NULL,
#                                  boundaryregion = boundaryregion, mesh = mesh, 
#                              prior.sigma = c(0.1,0.8), prior.range = c(rho, 0.9), covariate = covariate, PS = T,q=8)
# plot_sp_2(PSgeoPD[[1]],model = 'PSgeo',range(PSgeoPD[[1]]$pred_ll,PSgeoPD[[1]]$pred_ul),boundaryregion)
 # save(PSgeoPD, file = 'US_pop_COV_psgeo_beta_2019.Rdata')

dppoint$ecprob_PSmeld <- PSmeldingPD[[1]][["ecprob"]]
dppoint$ecprob_meld <-  MeldingPD[[1]][["ecprob"]]
dppoint$ecprob_PSgeo <- PSgeoPD[[1]][["ecprob"]]
range = range(dppoint$ecprob_PSmeld,dppoint$ecprob_PSgeo,dppoint$ecprob_meld)
boundaryregion_plot <- boundaryregion
plot_sp_3(dppoint, range = range)

# Data Creating ------------------------------------------------------
################# Areal data

str <- '~/Documents/Project2/Preferential Sampling 2/data/usadata/sdei-global-annual-gwr-pm2-5-modis-misr-seawifs-aod-v4-gl-03-2019.tif'
fa <- 125
ra <- fnTifdata(str, boundaryregion = usa%>%st_crop(usa, xmin =min(lon), xmax = max(lon), ymin = min(lat), ymax = max(lat)), fa)
plot(ra)
spol <- rasterToPolygons(ra, dissolve = F)
dearea <- st_as_sf(spol)%>% st_make_valid() %>% st_transform(crsproj)
colnames(dearea) <- c('value','geometry')

# # depoint 
# daily_pm_2019 <- read.csv("~/Documents/Project2/Preferential Sampling 2/data/daily_pm2_2019.csv")
# daily_pm_2019$Date_num <- as_date(ymd(daily_pm_2019$Date.Local))
# data <- daily_pm_2019 %>% dplyr::select(6,7,13,17) %>%  
#   group_by(Longitude,Latitude) %>% summarise(value = mean(Arithmetic.Mean))
# 
# 
# depoint <- data %>% st_as_sf(coords = c("Longitude", "Latitude"), dim = "XY") %>%
#   st_set_crs(4326) %>% st_crop(usa, xmin =min(lon), xmax = max(lon), ymin = min(lat), ymax = max(lat))
# depoint <- depoint%>%
#   st_transform(crsproj)
# figure_misdata(dearea,depoint,boundaryregion, title = 'Annual PM2.5 in 2019', unit = 'μg/m3')
# dppoint 
# dppoint <- fncreatedp(boundaryregion = boundaryregion, crsproj = crsproj)
# population_sf <-  st_read('~/Documents/Project2/Preferential Sampling 2/PSmelding/data/population.shp')
# colnames(population_sf) <- c('population_density','geometry')
# depoint <- depoint %>% st_join(population_sf) %>% select(value,population_density)
# # county_population <- st_read('~/Documents/Project2/Preferential Sampling 2/data/Population-Density By County/County.shp')%>% st_crop(usa, xmin =min(lon), xmax = max(lon), ymin = min(lat), ymax = max(lat))%>%st_transform(crsproj) %>%  st_as_sf() %>% 
# #   dplyr::select(B01001_cal,NAME)
# dppoint <- dppoint %>% st_join(population_sf) %>%  dplyr::select(population_density)
# save(depoint,dearea,dppoint,boundaryregion,covariate,mesh,file = "~/Documents/Project2/Preferential Sampling 2/PSmelding/data/Usa_pm_data.RData")

tif_file_path <- "data/usa_pd_2020_1km_UNadj.tif"
bd_ws84 <- boundaryregion %>% st_transform(4326)
population <- fnTifdata(tif_file_path,bd_ws84,10)
plot(population)
# spol <- rasterToPolygons(population, dissolve = F)
# population_sf <- st_as_sf(spol)%>% st_transform(crsproj)
# colnames(population_sf) <- c('population_density','geometry')
# # summary(population_sf$value)
# # covariates at observed points
# colnames(depoint_cov_pop) <- c('value','population_density','NAME','geometry','longitude')
# colnames(dppoint) <- c('population_density','NAME','geometry', 'longitude')
# # covariates at mesh nodes
# # create sf obj

# create areal covariate --------------------------------------------------
de_new <- dearea
de_new$group <- 1:nrow(de_new)
de_new <- de_new %>%
  st_join(population_sf)

de_new$area <- as.numeric(st_area(de_new)) %>% na.omit()

de_new <- de_new%>%
  group_by(group,value)%>%
  summarise(area_sum = sum(area),
            population_density = weighted.mean(population_density,area/sum(area))) %>% dplyr::select(-1)
dearea <- de_new %>% select(value,population_density)

st_write(dearea_cov, "data/dearea_cov.shp")
st_write(dppoint,'data/dppoint_cov.shp')
st_write(depoint_cov_pop,'data/depoint_cov.shp')
st_write(county_population,'data/county_population.shp')
st_write(population_sf,'data/population.shp')


# -------------------------------------------------------------------------
dppoint <- dppoint %>% st_join(population_sf) %>%  dplyr::select(population_density)
depoint <- depoint %>% st_join(population_sf) %>%  dplyr::select(value,population_density)
de_new <- dearea
de_new$group <- 1:nrow(de_new)
de_new <- de_new %>%
  st_join(population_sf)
de_new$area <- as.numeric(st_area(de_new)) %>% na.omit()

de_new <- de_new%>%
  group_by(group,value)%>%
  summarise(area_sum = sum(area),
            population_density= weighted.mean(population_density,area/sum(area))) %>% dplyr::select(-1)
dearea_cov <- de_new

st_write(dearea_cov, "data/dearea_world_pop.shp")
st_write(dppoint,'data/dppoint_world_pop.shp')
st_write(depoint,'data/depoint_world_pop.shp')

colnames(mesh_cov) <- c('population_density_county','geometry')
mesh_cov <- mesh_cov %>%
  mutate(population_density_county = ifelse(is.na(population_density_county), 0, population_density_county))
colnames(depoint) <- c('value','population_density_county','geometry')
colnames(dearea) <- c('value','area','population_density_county','geometry')
colnames(dppoint) <- c('population_density_county','geometry')
