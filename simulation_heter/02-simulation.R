
# Take measurements at points and areas
lpa <- fnMeasurementsatPointsAndAreas_heter(pnumm, anumm, ra, rp, seed, beta1 = beta1, beta0 = beta0 , sig.err = sig.err)
depoint <- lpa[1][[1]]
dearea <- lpa[2][[1]]



if(wantplot == TRUE){
  # plot of true surface with the sampled data points 
  latent_plot(ra, rp, depoint, dearea)
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
  # fnPlotLatentAndPrediction(r, rs, depoint, dearea, res, names(res)[4], res[[4]][[1]])
}


print("Calculate errors")
# Calculate errors
# truesurface at prediction locations
truesurface <- raster::extract(ra, as.matrix(st_coordinates(dppoint))[, c(1,2)])
truesurface_sf <- dppoint %>% mutate(true = truesurface)

# MSE and MAE
ME <- fnEvaluation_test(res, truesurface_sf, pnum, anum, sig.err = sig.err)
regexp <- "([0-9]+)\\.([0-9]*)"
time <- as.numeric(str_extract(unlist(res[['time']]), regexp))
Time <- as.data.frame(cbind(Geostat = time[1], PS_geo = time[2], Melding = time[3], PS_melding = time[4]))
estimate <- fnestimate(res)
print("Save files with errors")
# Name to save file with results
# paramname <- gsub("\\.", "p", paste(param))
# paramname <- paste(paramname, collapse = "-")
filesave <- paste0(paramname, "-", "NPoints", pnumm, "NAreas", anumm)
testcon <- file.exists(paste0("results/ME", filesave, ".csv"))

# Save files with errors of each fitted model
if(!testcon){write.table(ME, row.names = F, file = paste0("results/ME", filesave, ".csv"))} else{write.table(ME, row.names = F, file = paste0("results/ME", filesave, ".csv"), append = T, col.names = F)}
if(!testcon){write.table(Time, row.names = F, file = paste0("results/Time", filesave, ".csv"))} else{write.table(Time, row.names = F, file = paste0("results/Time", filesave, ".csv"), append = T, col.names = F)}
# if(!testcon){write.table(estimate[["Geostat"]], row.names = T, file = paste0("results/Est_geo", filesave, ".csv"))} else{write.table(estimate[["Geostat"]], row.names = T, file = paste0("results/Est_geo", filesave, ".csv"), append = T, col.names = F)}
if(!testcon){write.table(estimate[["PS_geo"]], row.names = T, file = paste0("results/Est_psgeo", filesave, ".csv"))} else{write.table(estimate[["PS_geo"]], row.names = T, file = paste0("results/Est_psgeo", filesave, ".csv"), append = T, col.names = F)}
if(!testcon){write.table(estimate[["Melding"]], row.names = T, file = paste0("results/Est_melding", filesave, ".csv"))} else{write.table(estimate[["Melding"]], row.names = T, file = paste0("results/Est_melding", filesave, ".csv"), append = T, col.names = F)}
if(!testcon){write.table(estimate[["PS_melding"]], row.names = T, file = paste0("results/Est_psmelding", filesave, ".csv"))} else{write.table(estimate[["PS_melding"]], row.names = T, file = paste0("results/Est_psmelding", filesave, ".csv"), append = T, col.names = F)}

# write.csv(vecmae, row.names = FALSE, file = paste0("results/MAE", filesave, ".csv"))


print("Finished")
