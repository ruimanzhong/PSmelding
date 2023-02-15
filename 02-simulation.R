#Parameters
nu = 1
mu1 <- param$mu1
mu0 <- param$mu0
nu0 <- param$nu
scl <- param$scl
scl0 <- param$scl0
sig2 <- param$sig2
sig20 <- param$sig20
sig.err <- param$sig.err
beta1 <- param$beta1
prior.sigma <- prior.sigma
prior.range <- prior.range
print(is.null(index))
# Generate surface
print("Generate surface")
lr <- fnGenerateSurface(xlim, ylim, by, mu1, mu0, nu, nu0, scl, scl0, sig2, sig20)
r <- lr[1][[1]]
rs <- lr[2][[1]]


print("Take measurements at points and areas")
# Take measurements at points and areas
lpa <- fnMeasurementsatPointsAndAreas(pnumm, anumm, r, rs, seed, beta1 = beta1, sig.err = sig.err)
depoint <- lpa[1][[1]]
dearea <- lpa[2][[1]]



if(wantplot == TRUE){
  # plot of true surface with the sampled data points 
  latent_plot(r, rs, depoint, dearea)
}

print("Fit models")
# Fit models
res <- fnFitModels(depoint = depoint, dearea = dearea, dppoint = dppoint , dparea = NULL,
                   boundaryregion, mesh = mesh, prior.sigma = prior.sigma, prior.range = prior.range, loc.d)

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
ME <- fnEvaluation(res, truesurface_sf, pnum, anum, index = index)

# for(i in 1:length(eva)){ 
#   print(i)
#   vecCRPS <- c(vecCRPS, myCRPS(eva[i])[[1]])
#   vecmae <- c(vecmae, myCRPS(eva[i])[[2]])
# }
# names(vecCRPS) <- names(res)
# names(vecmae) <- names(res)
# 
# print(pnumm)
# print(anumm)
# 
# vecCRPS
# vecmae
# 
print("Save files with errors")
# Name to save file with results
# paramname <- gsub("\\.", "p", paste(param))
# paramname <- paste(paramname, collapse = "-")
filesave <- paste0(paramname, "-", "NPoints", pnumm, "NAreas", anumm)
testcon <- file.exists(paste0("results/ME", filesave, ".csv"))
# Save files with errors of each fitted model
if(!testcon){write.table(ME, row.names = F, file = paste0("results/ME", filesave, ".csv"))} else{write.table(ME, row.names = F, file = paste0("results/ME", filesave, ".csv"), append = T, col.names = F)}
# write.csv(vecmae, row.names = FALSE, file = paste0("results/MAE", filesave, ".csv"))


print("Finished")
