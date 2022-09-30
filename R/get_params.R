#' Get parameters from model
#'
#' @param inputData
#' @param timeTreatment
#' @param upperLimit
#' @param upperLimitThreshold
#' @param orderConc
#' @param saveModel
#'
#' @return
#'
#' @examples
get_params <- function(inputData,
                       timeTreatment,
                       upperLimit,
                       upperLimitThreshold,
                       orderConc,
                       saveModel){

  max_k <- NA

  time <- list()

  models = list()

  time$points <- as.numeric(as.character(unique(inputData$time)))

  time$max <- max(time$points)

  time$min <- min(time$points)

  params <- matrix(data = NA, ncol = 5, nrow = length(time$points))

  colnames(params) <- c("h", "l_asymp","u_assymp", "coef_e", "p_val")

  rownames(params) <- time$points

  time$limits <- stats::setNames(rep(0,
                              times = length(time$points)),
                          time$points)

  time$prior <-  rep(1,length(time$points)) / length(time$points)  #uniform prior

  time$prior <- stats::setNames(time$prior, time$points)

  time$posterior <- stats::setNames(rep(0,
                                 times = length(time$points)),
                             time$points)

  tmp.time <- inputData[inputData$time == time$max,]

  conc <- tmp.time$concentration

  fc_ttm <- tmp.time$fc_ttm

  if(orderConc == T){conc = conc[order(fc_ttm, decreasing = T)]}

  tmp.time$concentration <- conc

  params[rownames(params) == time$max,] <-  try(get_logistic_fit(inputData = tmp.time,
                                                                 upperLimit = upperLimitThreshold, saveModel = F),silent = T)

  if(params[rownames(params) == time$max,2] >= upperLimitThreshold | is.na(params[rownames(params) == time$max,2]) |
     params[rownames(params) == time$max,2] >= upperLimit){

    params[rownames(params) == time$max,2] <- upperLimit

  }else{

    lapply(time$points, function(time_point){

      inputData =  inputData[inputData$time == time_point,]

      conc <- inputData$concentration

      fc_ttm <- inputData$fc_ttm

      if(orderConc == T){conc = conc[order(fc_ttm, decreasing = T)]}

      inputData$concentration <- conc

      output <- try(get_logistic_fit(inputData =  inputData,
                                      upperLimit = upperLimitThreshold,
                                      saveModel = saveModel),
                    silent = T)

    }) -> params

    if(saveModel==T){

      lapply(params, function(x){
        if(length(x)==2){
          return(list(models <- x[[1]],params <- x[[2]]))
        }else if(length(x)==1){
          return(list(models <- NULL,params <- x[[1]]))
        }

      }) -> params

      models <- lapply(params, "[[",1)
      params <- lapply(params, "[[",2)

    }else{
      params <- params
      }

    params <- do.call(rbind, params)

    rownames(params) <- time$points

  }

  ##### checking when model is undefined ####

  return.l_assymp <- params[,2] > upperLimit

  return.l_assymp <- time$points[return.l_assymp]

  return.l_assymp <- max(as.numeric(return.l_assymp), na.rm = T)

  return.na <- is.na(as.numeric(params[,2]))

  return.na <- time$points[return.na]

  return.na <- max(as.numeric(return.na), na.rm = T)

  return.sign <- c(0, diff(sign(as.numeric(params[,2]))))

  return.sign <- which(return.sign > 0)

  return.sign <- time$points[return.sign]

  return.sign <- max(as.numeric(return.sign), na.rm = T)

  time$return <- max(c(return.l_assymp, return.na, return.sign))

  if(time$return < timeTreatment){time$return = timeTreatment}

  if(saveModel==T){
    return(list(params, estimated_onset = time$return, models))
  }else{
    return(list(params, estimated_onset = time$return))
  }

}

