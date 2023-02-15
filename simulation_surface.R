library("parSim")

Results <- parSim(
  # Any number of conditions:
  pnum <- c(10),
  anum <- c(1),
  scl = c(0.05),
  scl0 = c(0.05),
  sig2 = c( 2 ),
  scig20 = c( 2 ), 
  beta1 = c(1),
  # Number of repititions?
  reps = 1,
  
  # Parallel?
  nCores = 1,
  
  # Write to file?
  write = T,
  name = c('PS_sim'),
  # Export objects (only needed when nCores > 1):
  
  # R expression:
  expression = {
    # Load all R packages in the expression if needed
    # library(...)
    
    # Want to debug? Enter browser() and run the function. Only works with nCores = 1!
    # browser()
    source('header.R')
    source('01-simulation.R')
    xlim <- c(0,1)
    ylim <- c(0,1)
    by <- 0.02
    nu = 1
    mu = 0
    mu1 = 4
    nu = 1
    nu0 = 1
    by = 0.02
    sig.err = 1
    # Enter whatever codes you want. I can use the conditions as objects.
    wantplot <- F
    print("Generate surface")
    lr <- fnGenerateSurface(xlim, ylim, by, mu1, mu, nu, nu0, scl, scl0, sig2, sig20, seed = seed)
    r <- lr[1][[1]]
    rs <- lr[2][[1]]
    
    
    print("Take measurements at points and areas")
    # Take measurements at points and areas
    lpa <- fnMeasurementsatPointsAndAreas(pnum = pnumm, anum = anumm, r, rs, seed, beta1 = beta1, sig.err = sig.err)
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
    ME <- fnEvaluation(res, truesurface_sf, pnum, anum)
    
    Results <- list(
      ME = ME
    )
    
    # Return:
    Results
  }
)
