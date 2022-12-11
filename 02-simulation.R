# Parameters

mu1 <- param$mu1 
mu0 <- param$mu0
nu <- param$nu
nu0 <- param$nu0
scl <- param$scl
scl0 <- param$scl0
sig2 <- param$sig2
sig20 <- param$sig20
sig.err <- param$sig.err
beta0 <- param$beta0
beta1 <- param$beta1


# Generate surface
print("Generate surface")
lr <- fnGenerateSurface(xlim, ylim, by, mu1, mu0, nu, nu0, scl, scl0, sig2, sig20)
r <- lr[1][[1]]
rs <- lr[2][[1]]


print("Take measurements at points and areas")
# Take measurements at points and areas
lpa <- fnMeasurementsatPointsAndAreas(pnumm, anumm, r, rs)
depoint <- lpa[1][[1]]
dearea <- lpa[2][[1]]



if(wantplot == TRUE){
  # plot of true surface with the sampled data points 
  latent_plot(r, rs, depoint, dearea)
}

print("Fit models")
# Fit models
res <- fnFitModels(depoint = depoint, dearea = dearea, dppoint = dppoint , dparea = NULL,
                   boundaryregion, mesh = mesh, prior.sigma = c(2, 0.1), prior.range = c(0.1, 0.8), loc.d)

if(wantplot == TRUE){
  # Plots prediction
  fnPlotLatentAndPrediction(r, rs, depoint, dearea, res, names(res)[1], res[[1]][[1]])
  fnPlotLatentAndPrediction(r, rs, depoint, dearea, res, names(res)[2], res[[2]][[1]])
  fnPlotLatentAndPrediction(r, rs, depoint, dearea, res, names(res)[3], res[[3]][[1]])
  fnPlotLatentAndPrediction(r, rs, depoint, dearea, res, names(res)[4], res[[4]][[1]])
}


print("Calculate errors")
# Calculate errors
# truesurface at prediction locations
truesurface <- raster::extract(r, as.matrix(st_coordinates(dppoint))[, c(1,2)])
truesurface_sf <- dppoint %>% mutate(true = truesurface)

# MSE and MAE
vecmse <- NULL
vecmae <- NULL
eva <- fnEvaluation(res, truesurface_sf, pnum, anum)
for(i in 1:length(eva)){ 
  print(i)
  vecmse <- c(vecmse, myMSE(eva[i])[[1]])
  vecmae <- c(vecmae, myMSE(eva[i])[[2]])
}
names(vecmse) <- names(res)
names(vecmae) <- names(res)

print(pnumm)
print(anumm)

vecmse
vecmae

print("Save files with errors")
# Name to save file with results
# paramname <- gsub("\\.", "p", paste(param))
# paramname <- paste(paramname, collapse = "-")
filesave <- paste0(paramname, "-", "NPoints", pnumm, "NAreas", anumm)

# Save files with errors of each fitted model
write.csv(vecmse, row.names = FALSE, file = paste0("results/MSE", filesave, ".csv"))
write.csv(vecmae, row.names = FALSE, file = paste0("results/MAE", filesave, ".csv"))


print("Finished")
