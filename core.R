#' From input data x, calculate trimmed means from replicates
#'
#' @param x inputData
#' @param k percent of observations to remove
#' @return The sum of \code{x} and \code{y}
tdsR_trim_mean = function(x, percent) {
  x = x[!is.na(x)]
  n = length(x)
  k = n*(percent/100)/2
  # round down if k is half an integer
  if(round(k) != k & round(k*2) == k*2) {
    lo = floor(k) + 1
    hi = n - lo + 1
  } else {
    lo = round(k) + 1
    hi = n - lo + 1
  }
  x = sort(x)[lo:hi]
  return(mean(x))
}

#' convert inputData A,B or C into grouped data for logictic fit
#'
#' @param inputData input data in format cases A,B or C
#' @param case specify the type of input data, case A, B or C
#' @return Organized data with controls and treatmets on the same row
tdsR_convert = function(inputData, case, initial_count) {

  if(case == "A" | case == "B") {
    return(inputData)
  } else if(case == "C") {
    delete_cols = which(colnames(inputData) %in% c('concentration',
                                                   'cell_count'))
    keys = colnames(inputData)[-delete_cols]
    time0 = inputData[inputData$time == 0, c(keys, 'cell_count')]
    ctrl = inputData[inputData$concentration == 0 & inputData$time > 0,
                     c(keys, 'cell_count')]
    data = inputData[inputData$concentration != 0 & inputData$time > 0, ]
    time0_keys = NULL
    ctrl_keys = NULL
    for(i in 1:length(keys)) {
      #i = 1
      time0_keys[i] = length(intersect(time0[[ keys[i] ]],
                                       data[[ keys[i] ]])) > 0
      ctrl_keys[i] = length(intersect(ctrl[[ keys[i] ]],
                                      data[[ keys[i] ]])) > 0
    }
    ctrl_keys = keys[ctrl_keys]

    time0_keys = keys[time0_keys]

    temp = ctrl[, ctrl_keys]
    ctrl$key = apply(temp, 1, function(x) paste(x, collapse = ' '))

    temp = time0[, time0_keys]
    time0$key = apply(temp, 1, function(x) paste(x, collapse = ' '))

    temp = data[, ctrl_keys]
    data$key_ctrl = apply(temp, 1, function(x) paste(x, collapse = ' '))

    temp = data[, time0_keys]
    data$key_time0 = apply(temp, 1, function(x) paste(x, collapse = ' '))

    data$cell_count__ctrl = NA
    data$cell_count__time0 = NA
    for(key in unique(ctrl$key)) {

      #key = unique(ctrl$key)[1]
      trimmed_mean = tdsR_trim_mean(ctrl[ctrl$key == key,]$cell_count, 50)
      data[data$key_ctrl == key, 'cell_count__ctrl'] = trimmed_mean
    }

    for(key in unique(time0$key)) {
      trimmed_mean = tdsR_trim_mean(time0[time0$key == key,]$cell_count, 50)
      data[data$key_time0 == key, 'cell_count__time0'] = trimmed_mean
    }

    delete_cols = which(colnames(data) %in% c('key_ctrl', 'key_time0'))
    data = data[, -delete_cols]

    if(!initial_count) { data$cell_count__time0 = NULL }
    data = as.data.frame(data)
    row.names(data) = 1:dim(data)[1]
    inputData = data
    return(inputData)
  }

}

#' Fit a logictic function in the inputData
#'
#' @param inputData A data.frame
#' @param groupingVariables Specify by which variables the data should be grouped
#' @param upperLimit Boundary for upper limit assymptote
#' @param lowerLimit Boundary for lower limit assymptote
#' @return Parameters for the logictic fit
tdsR_logistic_fit <- function(inputData, groupingVariables, upperLimit, lowerLimit){

  if(!is.null(upperLimit)){upperLimit <- c(NA,NA, upperLimit, NA)}

  if(!is.null(lowerLimit)){lowerLimit <- c(NA,NA, lowerLimit, NA)}

  output_drc <- drc::drm(
    formula = fc_ttm ~ concentration,
    na.action = na.omit,
    data = inputData,
    upperl = upperLimit,
    lowerl = lowerLimit,
    fct = drc::L.4())

  if(class(output_drc) == "drc" &&
     !is.null(stats::coef(output_drc)) &&
     !is.null(stats::residuals(output_drc))
  ) {

     output_df <- coef(output_drc)

     output_df["p_val"] <- summary(output_drc)[3]$coefficients[1,4]


     }else{

    output_df <- NA

  }

  return(output_df)

}

#' Handle the output of logictic fit and return tds together with logictic parameters
#'
#' @param inputData A data.frame
#' @param timeTreatment At what time the intervention was introduced
#' @param upperLimit Boundary for upper limit assymptote
#' @param lowerLimit Boundary for lower limit assymptote
#' @param limitThreshold Minimal cut off to run tdsR, if bellow threshold function return largest tds
#' @param orderConc Order concentration according to effect size
#' @param case which input data is provided: case A,B or C
#' @return tds and logictic parameters
tdsR_get_params <- function(inputData,
                            timeTreatment,
                            upperLimit,
                            lowerLimit,
                            limitThreshold,
                            orderConc,
                            case){

  time <- list()

  time$points <- as.numeric(as.character(unique(inputData$time)))

  time$max <- max(time$points)

  time$min <- min(time$points)

  params <- matrix(data = NA, ncol = 5, nrow = length(time$points))

  if(case == "B"){colnames(inputData)[which(colnames(inputData) == "value")] = "fc_ttm"}

  tmp.time <- inputData[inputData$time == time$max,]

  conc <- tmp.time$concentration

  fc_ttm <- tmp.time$fc_ttm

  if(orderConc == T){conc = conc[order(fc_ttm, decreasing = F)]}

  tmp.time$concentration <- conc

  colnames(params) <- c("h", "l_asymp","u_assymp", "coef_e", "p_val")

  rownames(params) <- time$points

  params[rownames(params) == time$max,] <-  try(tdsR_logistic_fit(inputData =  tmp.time, upperLimit = NULL, lowerLimit = limitThreshold), silent = T)

  params <- apply(params,2,as.numeric)

  colnames(params) <- c("h", "l_asymp","u_assymp", "coef_e", "p_val")

  rownames(params) <- time$points

  if(is.na(params[rownames(params) == time$max,"u_assymp"])){

    params[rownames(params) == time$max,3] <- limitThreshold

  }else{

    lapply(time$points, function(time_point){

      #time_point <- 60

      inputData =  inputData[inputData$time == time_point,]

      conc <- inputData$concentration

      fc_ttm <- inputData$fc_ttm

      if(orderConc == T){conc = conc[order(fc_ttm, decreasing = F)]}

      inputData$concentration <- conc

      output <- try(tdsR_logistic_fit(inputData = inputData, lowerLimit = lowerLimit, upperLimit = NULL), silent = T)

    }) -> params

    params <- do.call(rbind, params)

  }

  params <- apply(params,2,as.numeric)

  colnames(params) <- c("h", "l_asymp","u_assymp", "coef_e", "p_val")

  rownames(params) <- time$points

  #### checking when model crashes ####

  return.l_assymp <- params[,3] <= lowerLimit

  return.l_assymp <- time$points[return.l_assymp]

  return.l_assymp <- max(as.numeric(return.l_assymp), na.rm = T)

  return.na <- is.na(params[,3])

  return.na <- time$points[return.na]

  return.na <- max(as.numeric(return.na), na.rm = T)

  return.sign <- c(0, diff(sign(params[,3])))

  return.sign <- which(return.sign > 0)

  return.sign <- time$points[return.sign]

  return.sign <- max(as.numeric(return.sign), na.rm = T)

  time$return <- max(c(return.l_assymp, return.na, return.sign))

  if(time$return < timeTreatment){time$return = timeTreatment}

  return(list(params, estimated_onset = time$return))

}

#' Remove noise from inputData
#'
#' @param inputData A data.frame
#' @param groupingVariables Specify by which variables the data should be grouped
#' @return output of smooth function for every group in groupingVariables
tdsR_smooth <- function(inputData, groupingVariables, metric){


  groupingVariables <- append(groupingVariables, "concentration")

  groups <- do.call(paste, inputData[,groupingVariables])

  lapply(split(inputData, groups), function(x){

    #print(x$cell_line[1])
    #print(x$agent[1])
    #x = split(inputData, groups)[[5]]

    out <- x[!duplicated(x$time),]

    rownames(out) <- out$time

    model <- mgcv::gam(get(metric) ~ s(time, bs = "cs"), data = x)

    timeInput <- data.frame(time = unique(x$time))

    out[order(match(rownames(out), timeInput$time)),metric] <- predict(model, timeInput)

    return(out)

    }) -> out

  out <- do.call(rbind, out)

  return(out)

}

#' Main function that handles input data and return tds
#'
#' @param inputData input data in format cases A,B or C
#' @param groupingVariables Specify by which variables the data should be grouped
#' @param case which input data is provided: case A,B or C
#' @param timeTreatment At what time the intervention was introduced
#' @param upperLimit Boundary for upper limit assymptote
#' @param smoothData If noise removal from replicates should be performed: recommended for better fit
#' @param lowerLimit Boundary for lower limit assymptote
#' @param orderConc Order concentration according to effect size
#' @param limitThreshold Minimal cut off to run tdsR, if bellow threshold function return largest tds
#' @return tdsR and logictic fit parameters
tdsR_fit <- function(inputData, groupingVariables, case = "C", timeTreatment, upperLimit = NULL,
                     smoothData = T, lowerLimit = NULL, orderConc = T, limitThreshold = NULL){

  #TODO option for user to calculate trapezoid estimates

  if(case == "B"){
    if(!"value" %in% colnames(inputData)){stop("incorrect input B")}
    metric = "value"
  }else if(case == "C"){
    if(!"cell_count" %in% colnames(inputData)){stop("incorrect input C")}
    metric = "cell_count"
  }else{
    stop("incorrect input")
  }

  if(smoothData == T){inputData = tdsR_smooth(inputData, groupingVariables, metric)}

  inputData <- tdsR_convert(inputData = inputData, case = case, initial_count = T)

  if(case != "B"){
    inputData$fc_ttm = with(inputData, cell_count/cell_count__ctrl)
    inputData$fc_ctr = with(inputData, cell_count__ctrl/cell_count__time0)
  }

  if("cell_line" %in% colnames(inputData)){

    inputData$cell_line <- as.character(inputData$cell_line)

  }

  # the idea is to fit, get k (slope) bsaed on random sampling, and limit the area of search based on that

  if(length(groupingVariables[!groupingVariables == "time"]) > 1){

    inputData$keys <- (base::do.call(paste, inputData[, groupingVariables[!groupingVariables == "time"]]))

  }else{

    inputData$keys <- inputData[, groupingVariables[!groupingVariables == "time"]]

  }

  keys <- unique(inputData$keys)

  output <- lapply(keys, function(key){

    print(key)

    #key = keys[1]

    subset_data <- subset(inputData, keys == key)

    #inputData <- subset_data

    tmp <- tdsR_get_params(inputData = subset_data, timeTreatment = timeTreatment, upperLimit = upperLimit, orderConc = orderConc, lowerLimit = lowerLimit, case = case, limitThreshold = limitThreshold)

    tmp[[1]] <- cbind(key, tmp[[1]])

    names(tmp[[2]]) <- key

    return(tmp)

  })

  params <- lapply(output, "[[",1)

  names(params) <- keys

  estimated_onset <- unlist(lapply(output, "[[",2))

  names(estimated_onset) <- keys

  return(list(params, estimated_onset))

}


tdsR_getOutput <- function(inputData, metric){

  if(metric == "tdsR"){

    #inputData = tmp

    out <- inputData[[2]]

    groups <- names(out)

    out <- data.frame(groups = names(out), tds = out)

  }else if(metric == "parameters"){

    out <- inputData[[1]]

    out <- do.call(rbind, out)

  }else{
    stop("metric not found")
  }

  return(out)

}


