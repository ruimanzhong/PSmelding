library(rgeos)
library(deldir)
library(sf)
library(RColorBrewer)
library(GeoModels)
library(anySim)
library(rgeos)
library(stars)
library(INLA)
library(gcKrig)
library(GeoModels)
library(tidyverse)

source('fnPredictMelding.R')
source('fnPredictDown.R')
source('fnPredictMeldingcox.R')
source("fnCreateMesh.R")
source('fnCheckInputsMelding.R')
source('fnCheckInputsDown.R')
source('samplegenerator.r')
source('~/Documents/project2/eva_visa.R')
#source('ps_sample.R')

xlim = c (0,1)
ylim = c(0,1)

xlim0 = c(0,1)
ylim0 = c(0,1)
by = 0.02
mu1 = 4
mu0 = 0
nu = 1
nu0 = 1
scl= 0.15
scl0 = 0.13
sig2 = 2
sig20 = 2
sig.err = 1
beta0 = 0.5
beta1 = 0.1

pnum = 50
anum = 2
#---------------------
r <- latt_generation(xlim, ylim, by, mu1, nu, scl, sig2, seed = 123) 
rs <- latt_generation(xlim0, ylim0, by, mu0, nu0, scl0, sig20, seed = 123)
 

plot(r)
plot(rs)


loc.d <- cbind(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0))
mesh <- inla.mesh.2d(loc.domain = loc.d, offset = c(0.1, 0.35), 
                     max.edge = c(0.1, 0.25), cutoff = 0.01)
plot(mesh)


# prediction point --------------------------------------------------------

win = owin(xrange = xlim, yrange = ylim)
boundaryregion <- sf::st_as_sf(win)
x <- seq(from = xlim[1] + (by / 2), to = xlim[2] - (by / 2), by = by)
y <- seq(from = ylim[1] + (by / 2), to = ylim[2] - (by / 2), by = by)
coord <- expand.grid(x = x, y = y)
coop_sf <- sf::st_as_sf(coop, coords = c('Var1','Var2'))
dppoint <- coop_sf %>% st_join(boundaryregion, left = FALSE)

truesurface <- raster::extract(r, as.matrix(st_coordinates(dppoint))[, c(1,2)])
truesurface_sf <- dppoint %>% mutate(true = truesurface)

#------------------------------------
# Sample observations in points and areas
# p1 <- punifsample(pnum, r)
loct <- pcoxsample(pnum, rs)
p3 <- datagenerator(loct, r) 
a1 <- areasample(anum, r) %>% st_make_valid()%>% st_set_crs(NA_crs_)
depoint <- p3 %>% st_as_sf(coords = c("x", "y"), dim = "XY") %>% st_cast("POINT")
latent_plot(r,rs,depoint)



#--------------------
# model
#---------------------
ori_pref <- fnPredictMelding(depoint = depoint, dearea = a1, dppoint = dppoint, dparea = NULL,
                               boundaryregion = boundaryregion, mesh = mesh, prior.sigma = c(2, 0.1), prior.range = c(0.1, 0.8))


cox_pref <- fnMeldingCox(depoint = depoint, dearea = a1, dparea = NULL,prior.sigma = c(2, 0.1), dppoint = dppoint,
                             loc.d = loc.d,
                             prior.range = c(0.05, 0.01),
                             boundaryregion = boundaryregion, mesh = mesh)



summary(ori_pref[[3]])
summary(cox_pref[[3]])

#------------------
# Evaluation
#------------------
res <- makeNamedList( ori_pref, cox_pref)

eva <- fnEvaluation(res,truesurface_sf, pnum, anum)
names <- names(res)
mse_list = NULL
mae_list = NULL
for( i in 1:length(eva)){ 
  print(i)
  mse <- myMSE(eva[i])[[1]]
  mae <- myMSE(eva[i])[[2]]
  mse_list <- c(mse_list, mse)
  mae_list <- c(mae_list, mae)
}
names(mse_list) <- names
names(mae_list) <- names

mse_list
mae_list

result1 <- list(mse = mse_list,mae = mae_list, pnum, anum)
result2 <- list(mse = mse_list,mae = mae_list, pnum, anum)
result3 <- list(mse = mse_list,mae = mae_list, pnum, anum)
result4 <- list(mse = mse_list,mae = mae_list, pnum, anum)
result5 <- list(mse = mse_list,mae = mae_list, pnum, anum)
# no-tune melding,other the same as 2, the estimation of ori the b0 is not good, cox 2.4,0.55 , ori 2.6, 0.4

#------
#------