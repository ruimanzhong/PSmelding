# PS_SITES ----------------------------------------------------------------
crsproj <-  "+proj=utm +zone=29 +nord +units=km"

world <- ne_countries(scale='medium',returnclass = 'sf')
UK <- subset(world, admin == "United Kingdom")

boundaryregion <- UK%>%st_transform(crsproj)
ggplot(boundaryregion)+ geom_sf()
loc.d <- st_coordinates(boundaryregion)[,c(1,2)]
# Mesh_covariate ----------------------------------------------------------
boundaryregion_sp <- as_Spatial(boundaryregion[,1])
maxedge <- fnMaxEdgeMeshFromBoundary(boundaryregion)
mesh <- inla.mesh.2d(loc.domain = loc.d,boundary = boundaryregion_sp, 
                     max.edge = c(maxedge/10, maxedge), cutoff = maxedge/25
)
plot(mesh) 

mesh_cov_1 <- fnCreatemeshcov(mesh,population_sf,crsproj)
mesh_cov_2 <- fnCreatemeshcov(mesh,urban_sf,crsproj)
mesh_cov <- st_join(mesh_cov_1,mesh_cov_2)
colnames(mesh_cov) <- c('population_density','site_type','geometry')
mesh_cov <- mesh_cov %>%
  mutate(population_density = ifelse(is.na(population_density), 0, population_density)) %>%
  mutate(site_type = ifelse(is.na(site_type), 0, site_type))
covariate <- data.frame(rbind(mesh_cov[,c(1,2)],depoint[,c(2,3)])) %>% st_as_sf()
ggplot(data = boundaryregion) + geom_sf() + geom_sf(data = covariate, aes(col = population_density), size = 2)

PSmeldingSiteTyoe <- fnPSMeldingSiteType(depoint = depoint, dearea = dearea_sitetype, dppoint = dppoint, dparea = NULL,
                                       boundaryregion = boundaryregion, mesh = mesh, 
                                       prior.sigma = NULL, prior.range = NULL, covariate = covariate)
summary(PSmeldingSiteTyoe[[2]])
plot_sp_2(PSmeldingSiteTyoe[[1]],model = 'PSMelding witn site type',c(0,13),boundaryregion)

PSmeldingPD <- fnPSMeldingPD(depoint = depoint, dearea = dearea_pd, dppoint = dppoint, dparea = NULL,
                                                       boundaryregion = boundaryregion, mesh = mesh, 
                                                       prior.sigma = NULL, prior.range = NULL, covariate = covariate)
summary(PSmeldingPD[[2]])
plot_sp_2(PSmeldingPD[[1]],model = 'PSMelding multi witn site type',c(0,15),boundaryregion)

# PSmelding_cov_pop <- fnMeldingcox_with_cov(depoint = depoint, dearea = dearea_cov, dppoint = dppoint, dparea = NULL,
#                                        boundaryregion = boundaryregion, mesh = mesh, 
#                                        prior.sigma = NULL, prior.range = NULL, covariate = covariate)
# summary(PSmelding_cov_pop[[2]])
site_type

PSmelding_cov_two_pop <- fnMeldingcoxMultiprocess_with_cov(depoint = depoint, dearea = dearea_pd, dppoint = dppoint, dparea = NULL,
                                                       boundaryregion = boundaryregion, mesh = mesh, 
                                                       prior.sigma = NULL, prior.range = NULL, covariate = covariate)
summary(PSmelding_cov_two_pop[[2]])
plot_sp_2(PSmelding_cov_two[[1]],model = 'PSMelding multi witn popdensity',c(0,15),boundaryregion)

# exceedance Prob ---------------------------------------------------------
index <- inla.stack.index(stack = PSmelding[[4]], tag = "pred1")$data
marg <- PSmelding[[3]]$marginals.fitted.values[index][[1]]

ecprob_PSmeld <- sapply(PSmelding[[3]]$marginals.fitted.values[index],
                        FUN = function(marg){1-inla.pmarginal(q = 8, marginal = marg)})

index <- inla.stack.index(stack = Ps_geo[[4]], tag = "pred1")$data
marg <- Ps_geo[[3]]$marginals.fitted.values[index][[1]]
ecprob_PSgeo <- sapply(Ps_geo[[3]]$marginals.fitted.values[index],
                       FUN = function(marg){1-inla.pmarginal(q = 8, marginal = marg)})

index <- inla.stack.index(stack = Melding[[4]], tag = "pred1")$data
marg <- Melding[[3]]$marginals.fitted.values[index][[1]]
excpro <- sapply(Melding[[3]]$marginals.fitted.values[index],
                 FUN = function(marg){1-inla.pmarginal(q = 8, marginal = marg)})
dppoint$ecprob_PSmeld <- ecprob_PSmeld
dppoint$ecprob_meld <- excpro
dppoint$ecprob_PSgeo <- ecprob_PSgeo
range = range(dppoint$ecprob_PSmeld,dppoint$ecprob_PSgeo,dppoint$ecprob_meld)
plot_sp_3(dppoint, range = range)

# data_reprocessing -------------------------------------------------------
urban_sf <- st_read('pop_uk/urban_sf.shp') %>% st_transform(crsproj)
population_sf <- st_read('pop_uk/pop_uk.shp') %>% st_transform(crsproj)
colnames(population_sf) <- c('population_density','geometry')
colnames(urban_sf) <- c('site_type','geometry')
urban_sf$site_type <- urban_sf$site_type -1
# Install and load the openair package
# install.packages("openair")
library(openair)

meta_data <- importMeta(source = "aurn", all = TRUE)
head(meta_data, 3)

selected_data <- meta_data %>%
  filter(variable == "PM2.5")
head(selected_data, 3)

selected_sites <- selected_data %>%
  select(code) %>%
  mutate_all(.funs = tolower)
selected_sites

data <- importAURN(site = selected_sites$code, year = 2019, pollutant = "pm2.5")
head(data, 5)

filtered_data <- meta_data %>%
  filter(variable == "PM2.5") %>%
  right_join(data, "code") %>%
  select(date, pm2.5, code, site.x, site_type, latitude, longitude, variable) %>%
  rename(site = site.x) %>% na.omit()
head(filtered_data, 5)
all_sites<- filtered_data %>%
  group_by(site) %>%
  summarize(value = mean(pm2.5, na.rm = TRUE), latitude = first(latitude), longitude = first(longitude), site_type = first(site_type))
head(all_sites, 5)
# add population and sitetype ----------------------------------------------------------
clean_data= all_sites %>% mutate(site_type = str_extract(site_type,"\\b(Urban|Rural)\\b"))
depoint <- st_as_sf(clean_data, coords = c("longitude", "latitude")) %>%st_set_crs(4326) %>% st_transform(crsproj)
dppoint <- fncreatedp(boundaryregion = boundaryregion, crsproj = crsproj) 
dppoint <- dppoint %>% st_join(population_sf) %>%  dplyr::select(population_density)
dppoint <- dppoint %>% st_join(urban_sf) %>%  dplyr::select(population_density,site_type)
depoint <- depoint %>% st_join(population_sf) %>%  dplyr::select(value,population_density,site_type) %>% 
  mutate(site_type = ifelse(site_type == 'Rural',0,1)) 
depoint <- na.omit(depoint)
de_new <- dearea
de_new$group <- 1:nrow(de_new)
de_new <- de_new %>%
  st_join(population_sf)
de_new$area <- as.numeric(st_area(de_new)) %>% na.omit()

de_new <- de_new%>%
  group_by(group,value)%>%
  summarise(area_sum = sum(area),
            population_density= weighted.mean(population_density,area/sum(area))) %>% dplyr::select(-1)
dearea_pd <- de_new
st_read('pop_uk/dearea_sitetype.shp') %>% st_transform(crsproj)
colnames(dearea) <- c('value','area','population_density','geometry')
st_write(depoint,'depoint.shp')
st_write(dppoint,'dppoint.shp')
save(depoint,dearea_pd,dppoint,population_sf,crsproj,boundaryregion,loc.d,file ="~/Documents/project 2/PSmelding/uk_data.RData")
save(dearea_cov, file = "~/Documents/project 2/PSmelding/uk_dearea_sitetype.RData")
