fnPredictMeldingPS <- function(depoint, dearea = NULL, dppoint = NULL , dparea = NULL, boundaryregion,
                               mesh = NULL, prior.sigma = NULL, prior.range = NULL, loc.d = NULL){
  
  
  # replicate ---------------------------------------------------------------
  
  
  # Use 1 for points and 2 for areas
  # datasets estimation
  de1 <- depoint
  de2 <- dearea
  # datasets prediction
  dp1 <- dppoint
  dp2 <- dparea
  
  # Check inputs
  fnCheckInputsMelding(de1, de2, dp1, dp2, boundaryregion)
  
  # Logical values indicating what datasets I have
  de1ToF <- !is.null(de1)
  de2ToF <- !is.null(de2)
  dp1ToF <- !is.null(dp1)
  dp2ToF <- !is.null(dp2)
  
  
  
  # Create spde and index
  if(!is.null(prior.sigma) & !is.null(prior.range)){
    fnCheckPrior(prior.sigma,prior.range)
    spde <- inla.spde2.pcmatern(mesh = mesh, prior.range =prior.range, prior.sigma =prior.sigma)
  }else{
    spde <- inla.spde2.matern(mesh = mesh, alpha = 2, constr = T)
  }
  indexs <- inla.spde.make.index("s", spde$n.spde)
  
  #------
  # Dual mesh construction (from inla book)
  #------  
 domainSP <- as_Spatial(boundaryregion)
  dmesh <- book.mesh.dual(mesh)
  
  w <- sapply(1:length(dmesh), function(i) {
    if (gIntersects(dmesh[i, ], domainSP)) {
      return(gArea(gIntersection(dmesh[i, ], domainSP)))
    } else {
      return(0)
    }
  })
  
  xy <- as.matrix(st_coordinates(de1)[, c(1, 2)])
  n <- nrow(xy)
  nv <- mesh$n
  y.pp <- rep(0:1, c(nv, n))
  e.pp <- c(w, rep(0, n))
  #--------
  # *********** The unsure part of the code ************
  #-------- 
  # Projection matrices for points (estimation point and prediction point)
  
  if(de1ToF){Ae1 <- inla.spde.make.A(mesh = mesh, loc = as.matrix(st_coordinates(de1)[ , c(1,2)]))
  imat <- Diagonal(nv, rep(1, nv))
  A.pp <- rbind(imat, Ae1)
  }
  if(dp1ToF){Ap1 <- inla.spde.make.A(mesh = mesh, loc = as.matrix(st_coordinates(dp1)[ , c(1,2)]))}
  
  
  # Create projection matrix A for areas (estimation area and prediction area)
  if(de2ToF){Ae2 <- fnProjectionMatrixArea(de2, mesh)}
  if(dp2ToF){Ap2 <- fnProjectionMatrixArea(dp2, mesh)}
  #e.area <- de2 %>% st_join(de1) %>% group_by(value.x) %>% summarize(e = n())
  
  # Create stk.full
  stk.e1 <- NULL
  stk.e1pp <- NULL
  stk.e2 <- NULL
  stk.p1 <- NULL
  stk.p2 <- NULL
  
  # estimation point (de1), estimation area (de2), prediction point (dp1), prediction area (dp2)
  
  if(de1ToF){stk.e1 <- inla.stack(tag = "est1", data = list(y = cbind(de1$value, NA), e = rep(0, n)),
                                  A = list(1, Ae1), effects = list(b0 = rep(1, n), i = 1:nv))
  stk.e1pp <- inla.stack(tag = "est1pp", data = list(y = cbind(NA, y.pp), e = e.pp),
                         A = list(1, A.pp), effects = list(b0pp = rep(1, nv + n), j = 1:nv))
  }
  if(de2ToF){stk.e2 <- inla.stack(tag = "est2", data = list(data.frame(y = cbind(de2$value, NA), e = rep(NA, nrow(de2)))),
                                  A = list(1, Ae2), effects = list(data.frame(b0 = rep(1, nrow(de2))), i = indexs$s))}
  
  if(dp1ToF){stk.p1 <- inla.stack(tag = "pred1", data = list(y = cbind(rep(NA, nrow(dp1)), NA), e = rep(NA, nrow(dp1))),
                                  A = list(1, Ap1), effects = list(data.frame(b0 = rep(1, nrow(dp1))), i = indexs$s))}
  
  if(dp2ToF){stk.p2 <- inla.stack(tag = "pred2", data = list(y = cbind(rep(NA, nrow(dp2)), NA), e = rep(NA, nrow(dp2))),
                                  A = list(1, Ap2), effects = list(data.frame(b0 = rep(1, nrow(dp2))), i = indexs$s))}
  
  # construct stack full with the data we have # stk.full <- inla.stack(stk.e1, stk.p1)
  stk.full <- do.call(inla.stack, list(stk.e1, stk.e1pp, stk.e2, stk.p1, stk.p2)[c(de1ToF, de1ToF, de2ToF, dp1ToF, dp2ToF)])
  
  
  
  ####################
  
  # Specify formula melding
  
  formula <- y ~ 0 + b0 + b0pp + f(i, model = spde) + f(j, copy = "i", fixed = FALSE)
  
  # Call inla()
  res <- inla(formula, family = c("gaussian", "poisson"), data = inla.stack.data(stk.full),
              E = inla.stack.data(stk.full)$e,
              control.compute = list(config = TRUE, return.marginals.predictor = TRUE),
              control.predictor = list(compute = TRUE, link = 1, A = inla.stack.A(stk.full)))
  # Retrieve predictions points
  # Predictions points
  if(dp1ToF){dp1 <- fnRetrievePredictions(stk.full, res, "pred1", dp1)}
  # Predictions areas
  if(dp2ToF){dp2 <- fnRetrievePredictions(stk.full, res, "pred2", dp2)}
  return(list(dp1, dp2, res))
  
}

book.mesh.dual <- function(mesh) {
  if (mesh$manifold == "R2") {
    ce <- t(sapply(1:nrow(mesh$graph$tv), function(i) {
      colMeans(mesh$loc[mesh$graph$tv[i, ], 1:2])
    }))
    library(parallel)
    pls <- mclapply(1:mesh$n, function(i) {
      p <- unique(Reduce("rbind", lapply(1:3, function(k) {
        j <- which(mesh$graph$tv[, k] == i)
        if (length(j) > 0) {
          return(rbind(
            ce[j, , drop = FALSE],
            cbind(
              mesh$loc[mesh$graph$tv[j, k], 1] +
                mesh$loc[mesh$graph$tv[j, c(2:3, 1)[k]], 1],
              mesh$loc[mesh$graph$tv[j, k], 2] +
                mesh$loc[mesh$graph$tv[j, c(2:3, 1)[k]], 2]
            ) / 2
          ))
        } else {
          return(ce[j, , drop = FALSE])
        }
      })))
      j1 <- which(mesh$segm$bnd$idx[, 1] == i)
      j2 <- which(mesh$segm$bnd$idx[, 2] == i)
      if ((length(j1) > 0) | (length(j2) > 0)) {
        p <- unique(rbind(
          mesh$loc[i, 1:2], p,
          mesh$loc[mesh$segm$bnd$idx[j1, 1], 1:2] / 2 +
            mesh$loc[mesh$segm$bnd$idx[j1, 2], 1:2] / 2,
          mesh$loc[mesh$segm$bnd$idx[j2, 1], 1:2] / 2 +
            mesh$loc[mesh$segm$bnd$idx[j2, 2], 1:2] / 2
        ))
        yy <- p[, 2] - mean(p[, 2]) / 2 - mesh$loc[i, 2] / 2
        xx <- p[, 1] - mean(p[, 1]) / 2 - mesh$loc[i, 1] / 2
      } else {
        yy <- p[, 2] - mesh$loc[i, 2]
        xx <- p[, 1] - mesh$loc[i, 1]
      }
      Polygon(p[order(atan2(yy, xx)), ])
    })
    return(SpatialPolygons(lapply(1:mesh$n, function(i) {
      Polygons(list(pls[[i]]), i)
    })))
  } else {
    stop("It only works for R2!")
  }
}



fnPSMeldingCOVTwo <- function(depoint, dearea = NULL, dppoint = NULL , dparea = NULL, boundaryregion,
                                  mesh = NULL, prior.sigma = NULL, prior.range = NULL, loc.d = NULL, covariate, PS = T, q = NULL){
  # Use 1 for points and 2 for areas
  # datasets estimation
  de1 <- depoint
  de2 <- dearea
  # datasets prediction
  dp1 <- dppoint
  dp2 <- dparea

  # Check inputs
  fnCheckInputsMelding(de1, de2, dp1, dp2, boundaryregion)

  # Logical values indicating what datasets I have
  de1ToF <- !is.null(de1)
  de2ToF <- !is.null(de2)
  dp1ToF <- !is.null(dp1)
  dp2ToF <- !is.null(dp2)


  # Create spde and index
  if(!is.null(prior.sigma) & !is.null(prior.range)){
    fnCheckPrior(prior.sigma,prior.range)
    spde <- inla.spde2.pcmatern(mesh = mesh, prior.range =prior.range, prior.sigma =prior.sigma)
  }else{
    spde <- inla.spde2.matern(mesh = mesh, alpha = 2, constr = T)
  }
  indexs <- inla.spde.make.index("s", spde$n.spde)

  #------
  # Dual mesh construction (from inla book)
  #------
  domainSP <- as_Spatial(boundaryregion)
  dmesh <- book.mesh.dual(mesh)

  w <- sapply(1:length(dmesh), function(i) {
    if (gIntersects(dmesh[i, ], domainSP)) {
      return(gArea(gIntersection(dmesh[i, ], domainSP)))
    } else {
      return(0)
    }
  })

  xy <- as.matrix(st_coordinates(de1)[, c(1, 2)])
  n <- nrow(xy)
  nv <- mesh$n
  y.pp <- rep(0:1, c(nv, n))
  e.pp <- c(w, rep(0, n))
  if(de1ToF){Ae1 <- inla.spde.make.A(mesh = mesh, loc = as.matrix(st_coordinates(de1)[ , c(1,2)]))
  imat <- Diagonal(nv, rep(1, nv))
  A.pp.c <- rbind(imat, Ae1)
  }
  if(dp1ToF){Ap1 <- inla.spde.make.A(mesh = mesh, loc = as.matrix(st_coordinates(dp1)[ , c(1,2)]))}
  if(de2ToF){Ae2 <- fnProjectionMatrixArea(de2, mesh)}

  # Create stk.full,
  #stk.e1 and stk.epp for geostatistical data and point process, stk.e2 for areal data
  stk.e1 <- NULL
  stk.e1pp <- NULL
  stk.e2 <- NULL
  stk.p1 <- NULL
  stk.p2 <- NULL
  if(de1ToF){
    stk.e1 <- inla.stack(tag = "est1", data = list(y = cbind(de1$value, NA), e = rep(0, n)),
                                  A = list(1,1,Ae1), effects = list(b0 = rep(1, nrow(de1)), covariate = de1$population_density, list(i = 1:nv)))
    stk.e1pp.c <- inla.stack(tag = "est1pp", data = list(y = cbind(NA, y.pp), e = e.pp),
                           A = list(1, A.pp.c), effects = list(list(b0pp = 1, covariate_pp = covariate$population_density), list(j = 1:nv)))
  }
  if(de2ToF){stk.e2 <- inla.stack(tag = "est2", data = list(data.frame(y = cbind(de2$value, NA), e = rep(NA, nrow(de2)))),
                                  A = list(1,1, Ae2), effects = list(b0 = rep(1, nrow(de2)), covariate = de2$population_density,list(k = 1:nv,s1 = 1:nv)))}

  if(dp1ToF){stk.p1 <- inla.stack(tag = "pred1", data = list(y = cbind(rep(NA, nrow(dp1)), NA), e = rep(NA, nrow(dp1))),
                                  A = list(1,1, Ap1), effects = list(b0 = rep(1, nrow(dp1)), covariate = dp1$population_density, i = 1:nv))}

  stk.full <- do.call(inla.stack, list(stk.e1, stk.e1pp.c, stk.e2, stk.p1)[c(de1ToF, PS, de2ToF, dp1ToF)])
  if(PS == T){
    if(de2ToF){formula <- y ~ 0 + b0 + b0pp  + covariate_pp + covariate + f(i, model = spde) + f(j, copy = "i", fixed = FALSE) + 
      f(k, copy = "i", fixed = T)
    }
    if(!de2ToF){formula <- y ~ 0 + b0 + b0pp + covariate_pp + covariate + f(i, model = spde) + f(j, copy = "i", fixed = FALSE) }
  } else {
    formula <- y ~ 0 + b0 + covariate + f(i, model = spde) + f(k, copy = "i", fixed = FALSE)
  }
  res <- inla(formula, family = c("gaussian", "poisson"), data = inla.stack.data(stk.full),
              E = inla.stack.data(stk.full)$e,
              control.compute = list(config = TRUE, return.marginals.predictor = TRUE),
              control.predictor = list(compute = TRUE, link = 1, A = inla.stack.A(stk.full)))
  print(summary(res))
  dp1 <- fnRetrievePredictions(stk.full, res, "pred1", dp1)
  if(!is.null(q)){  index <- inla.stack.index(stack = stk.full, tag = "pred1")$data
  ecprob <- sapply(res$marginals.fitted.values[index],
                   FUN = function(marg){1-inla.pmarginal(q = q, marginal = marg)})
  dp1$ecprob <- ecprob}
  return(list(dp1, res))
}

fnPSMeldingCOVSV <- function(depoint, dearea = NULL, dppoint = NULL , dparea = NULL, boundaryregion,
                              mesh = NULL, prior.sigma = NULL, prior.range = NULL, loc.d = NULL, covariate, PS = T, q = NULL){
  # Use 1 for points and 2 for areas
  # datasets estimation
  de1 <- depoint
  de2 <- dearea
  # datasets prediction
  dp1 <- dppoint
  dp2 <- dparea
  
  # Check inputs
  fnCheckInputsMelding(de1, de2, dp1, dp2, boundaryregion)
  
  # Logical values indicating what datasets I have
  de1ToF <- !is.null(de1)
  de2ToF <- !is.null(de2)
  dp1ToF <- !is.null(dp1)
  dp2ToF <- !is.null(dp2)
  
  
  # Create spde and index
  if(!is.null(prior.sigma) & !is.null(prior.range)){
    fnCheckPrior(prior.sigma,prior.range)
    spde <- inla.spde2.pcmatern(mesh = mesh, prior.range =prior.range, prior.sigma =prior.sigma)
  }else{
    spde <- inla.spde2.matern(mesh = mesh, alpha = 2, constr = T)
  }
  indexs <- inla.spde.make.index("s", spde$n.spde)
  
  #------
  # Dual mesh construction (from inla book)
  #------
  domainSP <- as_Spatial(boundaryregion)
  dmesh <- book.mesh.dual(mesh)
  
  w <- sapply(1:length(dmesh), function(i) {
    if (gIntersects(dmesh[i, ], domainSP)) {
      return(gArea(gIntersection(dmesh[i, ], domainSP)))
    } else {
      return(0)
    }
  })
  
  xy <- as.matrix(st_coordinates(de1)[, c(1, 2)])
  n <- nrow(xy)
  nv <- mesh$n
  y.pp <- rep(0:1, c(nv, n))
  e.pp <- c(w, rep(0, n))
  if(de1ToF){Ae1 <- inla.spde.make.A(mesh = mesh, loc = as.matrix(st_coordinates(de1)[ , c(1,2)]))
  imat <- Diagonal(nv, rep(1, nv))
  A.pp.c <- rbind(imat, Ae1)
  }
  if(dp1ToF){Ap1 <- inla.spde.make.A(mesh = mesh, loc = as.matrix(st_coordinates(dp1)[ , c(1,2)]))}
  if(de2ToF){Ae2 <- fnProjectionMatrixArea(de2, mesh)}
  
  # Create stk.full,
  #stk.e1 and stk.epp for geostatistical data and point process, stk.e2 for areal data
  stk.e1 <- NULL
  stk.e1pp <- NULL
  stk.e2 <- NULL
  stk.p1 <- NULL
  stk.p2 <- NULL
  if(de1ToF){
    stk.e1 <- inla.stack(tag = "est1", data = list(y = cbind(de1$value, NA), e = rep(0, n)),
                         A = list(1,1,Ae1 * de1$population_density,Ae1), effects = list(b0 = rep(1, nrow(de1)), covariate = de1$population_density,k = 1:nv, list(i = 1:nv)))
    stk.e1pp.c <- inla.stack(tag = "est1pp", data = list(y = cbind(NA, y.pp), e = e.pp),
                             A = list(1, A.pp.c * covariate$population_density, A.pp.c), effects = list(list(b0pp = 1, covariate_pp = covariate$population_density),l = 1:nv,list(j = 1:nv)))
  }
  if(de2ToF){stk.e2 <- inla.stack(tag = "est2", data = list(data.frame(y = cbind(de2$value, NA), e = rep(NA, nrow(de2)))),
                                  A = list(1,1,Ae2 * de2$population_density, Ae2), effects = list(b0 = rep(1, nrow(de2)), covariate = de2$population_density,k = 1:nv,list(i = 1:nv, s2 = 1:nv)))}
  
  if(dp1ToF){stk.p1 <- inla.stack(tag = "pred1", data = list(y = cbind(rep(NA, nrow(dp1)), NA), e = rep(NA, nrow(dp1))),
                                  A = list(1,1,Ap1* dp1$population_density, Ap1), effects = list(b0 = rep(1, nrow(dp1)), covariate = dp1$population_density,k = 1:nv, i = 1:nv))}
  
  stk.full <- do.call(inla.stack, list(stk.e1, stk.e1pp.c, stk.e2, stk.p1)[c(de1ToF, PS, de2ToF, dp1ToF)])
  if(PS == T){
    if(de2ToF){formula <- y ~ 0 + b0 + b0pp  + covariate_pp + covariate + f(i, model = spde) + f(k, model = spde) + f(j, copy = "i", fixed = FALSE) + f(l, copy = "k", fixed = FALSE)  + f(s2, model = spde)}
    if(!de2ToF){formula <- y ~ 0 + b0 + b0pp + covariate_pp + covariate + f(i, model = spde) + f(j, copy = "i", fixed = FALSE) }
  } else {
    formula <- y ~ 0 + b0 + covariate + f(i, model = spde) + f(s2, model = spde)+ f(s1, model = 'iid')
  }
  res <- inla(formula, family = c("gaussian", "poisson"), data = inla.stack.data(stk.full),
              E = inla.stack.data(stk.full)$e,
              control.compute = list(config = TRUE, return.marginals.predictor = TRUE),
              control.predictor = list(compute = TRUE, link = 1, A = inla.stack.A(stk.full)))
  print(summary(res))
  dp1 <- fnRetrievePredictions(stk.full, res, "pred1", dp1)
  if(!is.null(q)){  index <- inla.stack.index(stack = stk.full, tag = "pred1")$data
  ecprob <- sapply(res$marginals.fitted.values[index],
                   FUN = function(marg){1-inla.pmarginal(q = q, marginal = marg)})
  dp1$ecprob <- ecprob}
  return(list(dp1, res))
}

fnCreatemeshcov<-function(mesh,covaraite, crsproj){
  sf_points <- st_as_sf(data.frame(X = mesh$loc[,1], Y = mesh$loc[,2]), coords = c("X", "Y"), crs = crsproj, agr = "constant")
  mesh_cov <- sf_points %>% st_join(covaraite)
  return(mesh_cov)
}

fncreatedp<- function(boundaryregion, crsproj){
  bb <- unname(attributes(st_geometry(boundaryregion))$bbox)
  # Grid
  x <- seq(bb[1] - 1, bb[3] + 1, length.out = 100)
  y <- seq(bb[2] - 1, bb[4] + 1, length.out = 100)
  coop <- expand.grid(x, y)
  coop_sf <- sf::st_as_sf(coop, coords = c('Var1','Var2'), crs = crsproj)
  dpcontsurface <- coop_sf %>% st_join(boundaryregion, left = FALSE)
  return(dpcontsurface)
}
