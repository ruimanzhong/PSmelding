fnEvaluation <- function(res, truesurface_sf, pnum, anum,index, sig.err) {
  # if(!"Cluster" %in% names(truesurface_sf)) {truesurface_sf$Cluster <- rep(1, nrow(truesurface_sf)) }
  len <- length(res)-1
  names <- names(res)[1:len]
  n <- nrow(truesurface_sf)
  ME <- lapply(names, function(name) return(as.data.frame (cbind(Index = index, Model = name, MSE = mean((res[[name]][[1]][["pred_mean"]] - truesurface_sf$true)^2), MAE = mean(abs(res[[name]][[1]][["pred_med"]] - truesurface_sf$true)),
                                                                 elpd = 0, 
                                                  WD = mean(((res[[name]][[1]][["pred_mean"]] - truesurface_sf$true)^2) + res[[name]][[1]][["sd"]]^2 + sig.err - 2* (sig.err* (res[[name]][[1]][["sd"]]^2)))))))
  
  
  MEE <- as.data.frame(rbind(ME[[1]],ME[[2]],ME[[3]]))
  return(MEE)
}

fnEvaluation_test <- function(res, truesurface_sf, pnum, anum, sig.err) {
  # if(!"Cluster" %in% names(truesurface_sf)) {truesurface_sf$Cluster <- rep(1, nrow(truesurface_sf)) }
  len <- length(res)-1
  names <- names(res)[1:len]
  n <- nrow(truesurface_sf)
  ME <- lapply(names, function(name) return(as.data.frame (cbind( Model = name, MSE = mean((res[[name]][[1]][["pred_mean"]] - truesurface_sf$true)^2), MAE = mean(abs(res[[name]][[1]][["pred_med"]] - truesurface_sf$true)),
                                                                 elpd = 0, 
                                                                 WD = mean(((res[[name]][[1]][["pred_mean"]] - truesurface_sf$true)^2) + res[[name]][[1]][["sd"]]^2 + sig.err - 2* (sig.err* (res[[name]][[1]][["sd"]]^2)))))))
  
  
  MEE <- as.data.frame(rbind(ME[[1]],ME[[2]],ME[[3]]))
  return(MEE)
}

fnestimate <-function(res){
  len <- length(res)-1
  names <- names(res)[1:len]
  e <- lapply(names, function(name) rbind(res[[name]][[3]][["summary.fixed"]][,c(1,2,3,5,6)], res[[name]][[3]][["summary.hyperpar"]][,c(1,2,3,5,6)]))
  names(e) <- names
  return(e)
}

# fn_microegodic_para <- function(res){
#   len <- length(res)-1
#   names <- names(res)[1:len]
#   e <- lapply(names, function(name) fn_hyper_combine(res[[name]][[3]]) )
#   
#   names(e) <- names
#   return(e)
# }
# 
# fn_hyper_combine <- function(inla_res){
#   tau <- NULL
#   
#   n <- length(inla_res[['misc']][['configs']][['config']])
#   for (i in 1:n)
#   {
#    hyper <-  as.data.frame(res[[name]][[3]][['misc']][['configs']][['config']][[i]][['theta']])
#    
#   }
#   return(margin_para)
# }
# Other -------------------------------------------------------------------

makeNamedList <- function(...) {
  structure(list(...), names = as.list(substitute(list(...)))[-1L])
}
