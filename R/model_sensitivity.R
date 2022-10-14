#' Create synthetic GRD data
#'
#' @param cell_tz parameter
#' @param t parameter
#' @param t_onset parameter
#' @param k parameter
#' @param maxeff parameter
#' @param halfeff parameter
#' @param conc parameter
#' @param hill parameter
#'
#' @return model of growth curve with GRD
#' @export
#'
#' @examples
#' #define params

#' k = 0.3
#' t_ttm <- 1
#' nreps <- 5
#' time <- seq(0,3,0.1)
#' conc <- matrix(base::rep(c(0,seq(0.5,8,length.out = 6)),  each = nreps), dimnames = list(NULL, "concentration"))
#'
#' params <- matrix(data = c(runif(nreps, min = 2, max = 2), seq(1,1.8,length.out = 5)), dimnames = list(NULL, c("halfeff", "t_onset")), nrow = nreps, ncol = 2)
#' params <- cbind(do.call("rbind", rep(list(params), 7)), conc)
#' params <- do.call(rbind, replicate(length(time), params, simplify=FALSE)) #replicate matrix to acommodate time
#' params <- cbind(params, time = rep(time, times = nreps))
#' #generate simulated data
#' sapply(1:dim(params)[1], function(x)
#'   model_growth(cell_tz = 5, t = params[x,'time'], t_onset = params[x,"t_onset"], k =  k, maxeff = 1.0, halfeff = params[x,"halfeff"], conc = params[x,"concentration"], hill = 1.6)
#'   ) -> sample_data

model_growth <- function(cell_tz, t, t_onset, k, maxeff, halfeff, conc, hill){

  maxeff <- ifelse(t_onset > t, 0, maxeff)

  if(t >= t_onset){

    cell_tz <- cell_tz * exp( ( t_onset * k ) )

    t <- t - t_onset

    return( cell_tz * exp( ( t * k *( 1 - ( (maxeff*conc ** hill )/ (halfeff ** hill + conc ** hill) ) ) ) ) )

  }else{

    return( cell_tz * exp( ( t * k *( 1 - ( (maxeff*conc ** hill )/ (halfeff ** hill  + conc ** hill) ) ) ) ) )

  }
}
