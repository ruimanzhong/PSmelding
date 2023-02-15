fnEvaluation <- function(res, truesurface_sf, pnum, anum,index, ...) {
  # if(!"Cluster" %in% names(truesurface_sf)) {truesurface_sf$Cluster <- rep(1, nrow(truesurface_sf)) }
  len <- length(res)-1
  names <- names(res)[1:len]
  n <- nrow(truesurface_sf)
  e <- lapply(names, function(name) res[[name]][[1]][["pred_mean"]] - truesurface_sf$true)
  names(e) <- names
  ME <- lapply(names, function(name) return(list (MSE = mean(e[[name]]^2), MAE = mean(abs(e[[name]])), elpd = fnELPD(res[[name]][[3]],paramname), 
                                                  WD = (sum(e[[name]]^2) + n*sig.err + sum(res[[name]][[1]][["sd"]]^2) - sum(2* sig.err*res[[name]][[1]][["sd"]]))/n)))
  
  
  regexp <- "([0-9]+)\\.([0-9]*)"
  time <- as.numeric(str_extract(unlist(res[['time']]), regexp))
  
  
  names(ME) <- names
  MEE <- as.data.frame(cbind(ID = index, Model = rep(names, each  =4), type = rep(c('MSE', 'MAE', 'elpd', 'WD'), 4), value  =  unname(unlist(ME))), time = rep(time, each  =4) )
  return(MEE)
  
}

fnELPD <- function(res,paramname){
  elpd <- try(mean(inla.group.cv(res)[["cv"]], na.rm = T), silent = T)
  if(class(elpd) == "try-error"){
    save(res,turesirface_sf, file = paste0("error/",res[[name]],paramname,pnum,anum,".RData"))
    elpd <- NA}
  return(elpd)
  
}

# myerrorD <- function(P,Py, y, name){
#   wd = wasserstein1d(P, y, p = 1, wa = NULL, wb = NULL)
#   crps_py = scoringutils::crps_sample(y, t(P))
#   crps_pp = scoringutils::crps_sample(Py, t(P))
#   scrps <- -0.5*(1+ crps_py/crps_pp + log(2* abs(crps_pp)))
#   metric <- list(scrps, wd)
#   names(metric) <- c(paste(name, 'scrps'), paste(name, 'WD'))
#   return(metric)
# }


# Other -------------------------------------------------------------------

makeNamedList <- function(...) {
  structure(list(...), names = as.list(substitute(list(...)))[-1L])
}
