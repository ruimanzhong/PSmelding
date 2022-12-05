fnMeldingCox <- function(depoint, dearea = NULL, dppoint = NULL , dparea = NULL, boundaryregion, loc.d, 
                         mesh = NULL, prior.sigma = NULL, prior.range = NULL) {
  # Use 1 for points and 2 for areas
  # datasets estimation
  de1 <- depoint
  de2 <- dearea
# datasets prediction
  dp2 <- dparea
  dp1 <- dppoint
  # Check inputs
  fnCheckInputsMelding(de1, de2, dp1, dp2, boundaryregion)

  # Logical values indicating what datasets I have
  de1ToF <- !is.null(de1)
  dp1ToF <- !is.null(dp1)
  de2ToF <- !is.null(de2)
  dp2ToF <- !is.null(dp2)


  # Create spde and index
  if (!is.null(prior.sigma) & !is.null(prior.range)) {
    fnCheckPrior(prior.sigma, prior.range)
    spde <- inla.spde2.pcmatern(mesh = mesh, prior.range = prior.range, prior.sigma = prior.sigma)
  } else {
    spde <- inla.spde2.matern(mesh = mesh, alpha = 2, constr = T) 
    
  }

  indexs <- inla.spde.make.index("s", spde$n.spde)
  domain.polys <- Polygons(list(Polygon(loc.d)), "0")
  domainSP <- SpatialPolygons(list(domain.polys))
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

  

  imat <- Diagonal(nv, rep(1, nv))
  Ae1 <- inla.spde.make.A(mesh, as.matrix(st_coordinates(de1)[, c(1, 2)]))

  A.pp <- rbind(imat, Ae1)
  if (dp1ToF) {
    N <- nrow(dp1)
    y.ppp <- rep(0, N)
    e.ppp <- rep(0, N)
    Ap1 <- inla.spde.make.A(mesh, as.matrix(st_coordinates(dp1)[, c(1, 2)]))
    A.ppp <- Ap1
  }
  # Create projection matrix A for areas (estimation area and prediction area)
  if (de2ToF) {
    Ae2 <- fnProjectionMatrixArea(de2, mesh)
    e.area <- de2 %>% st_join(de1) %>% group_by(value.x) %>% summarize(e = n())
  }
  if (dp2ToF) {
    Ap2 <- fnProjectionMatrixArea(dp2, mesh)
  }

  # Create stk.full
  stk.e1 <- NULL
  stk.e2 <- NULL
  stk.p1 <- NULL
  stk.p2 <- NULL

  # estimation point (de1), estimation area (de2), prediction point (dp1), prediction area (dp2)

  stk2.y <- inla.stack(
    data = list(y = cbind(de1$value,NA), e = rep(0, n)),
    A = list(Ae1, 1),
    effects = list(i = 1:nv, b0.y = rep(1, n)),
    tag = "est1"
  )

  stk2.pp <- inla.stack(
    data = list(y = cbind(NA, y.pp), e = e.pp),
    A = list(A.pp, 1),
    effects = list(j = 1:nv, b0.pp = rep(1, nv + n)),
    tag = "pp1"
  )
  if (de2ToF) {
    stk.e2 <- inla.stack(
      tag = "est2",
      data = list(y = cbind(de2$value, NA)),
      A = list(1, Ae2), effects = list(data.frame(b0.y = rep(1, nrow(de2))), i = indexs$s)
    )
  }


  if (dp1ToF) {
    
    stk.p1 <- inla.stack(
      tag = "pred1", data = list(cbind(dp1, NA)),
      A = list(1, Ap1),
      effects = list(b0.y = rep(1, nrow(dp1)), i = 1:nv)
    )
  }
  if (dp2ToF) {
    stk.p2 <- inla.stack(tag = "pred2", data = list(y = NA), A = list(1, Ap2), effects = list(data.frame(b0 = rep(1, nrow(dp2))), i = indexs$s))
  }
  # construct stack full with the data we have # stk.full <- inla.stack(stk.e1, stk.p1)
  stk.full <- do.call(inla.stack, list(stk2.y, stk2.pp, stk.e2, stk.p1)[c( T, T, de2ToF, dp1ToF)])

  # Specify formula melding

  jform <- y ~ 0 + b0.y + b0.pp + f(i, model = spde) +
    f(j,copy = "i", fixed = FALSE)

  # Call inla()
  res <- inla(jform,
    family = c("gaussian", "poisson"),
    data = inla.stack.data(stk.full),
    E = inla.stack.data(stk.full)$e,
    control.predictor = list(A = inla.stack.A(stk.full))
  )

  # Retrieve predictions points
  # Predictions points
  if (dp1ToF) {
    dp1 <- fnRetrievePredictions(stk.full, res, "pred1", dp1)
  }
  # Predictions areas
  if (dp2ToF) {
    dp2 <- fnRetrievePredictions(stk.full, res, "pred2", dp2)
  }

  return(list(dp1, dp2,res
  ))
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
