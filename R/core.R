# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

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

tdsR_convert = function(inputData, case, initial_count) {
  #FIXME this function was taken from GRmetrics package, give credits or change

  if(case == "A") {
    return(inputData)
  } else if(case == "C") {
    delete_cols = which(colnames(inputData) %in% c('concentration',
                                                   'cell_count'))
    keys = colnames(inputData)[-delete_cols]
    time0 = inputData[inputData$time == 0, c(keys, 'cell_count')]
    ctrl = inputData[inputData$concentration == 0 & inputData$time > 0,
                     c(keys, 'cell_count')]
    data = inputData[inputData$concentration != 0 & inputData$time > 0, ]
    time0_keys = NULL   #TODO CONTINUE HERE
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

tdsR_logistic_fit <- function(inputData, groupingVariables,
                              upperLimit, lowerLimit = 1E-10,
                              saveModel){

  #inputData <- tmp.time


  if(!is.null(upperLimit)){upperLimit <- c(NA,NA, upperLimit, NA)}

  if(!is.null(lowerLimit)){lowerLimit <- c(NA,NA, lowerLimit, NA)}

  output_drc <- drc::drm(
    formula = as.numeric(fc_ttm) ~ as.numeric(concentration),
    na.action = na.omit,
    data = inputData,
    upperl = upperLimit,
    lowerl = lowerLimit,
    fct = drc::L.4())

  # output_drc <- try(drc::drm(
  #   formula = fc_ttm ~ concentration,
  #   na.action = na.omit,
  #   data = inputData,
  #   fct = drc::l3u(names = c("h","l_asymp", "half_max")),
  #   upperl = c(NA, upperLimit, NA),
  #   lowerl = c(NA, 1E-10, NA)),
  #   silent = T)

    # output_lm <-  try(lm(
    #   formula = fc_ttm ~ offset(concentration),
    #   data = inputData
    # ), silent = T)

  # test if logictic fit outperforms linear regression using ANOVA

  if(class(output_drc) == "drc" &&
     !is.null(stats::coef(output_drc)) &&
     !is.null(stats::residuals(output_drc))
  ) {

    # RSS1 <-  sum((stats::resid(output_lm))**2)
    #
    # RSS2 <-  sum((stats::resid(output_drc))**2)
    #
    # nparam1 <- 2  #FIXME check if dfs and models are correct
    #
    # nparam2 <- 3
    #
    # RDF1 <- dim(output_drc$data)[1] - nparam1
    #
    # RDF2 <- dim(output_drc$data)[1] - nparam2
    #
    # f_value = (RSS1 - RSS2)/(RDF1-RDF2) / (RSS2)/(RDF2)
    #
    # f_pval <- stats::pf(f_value, RDF1, RDF2, lower.tail = F)
    #
     output_df <- coef(output_drc)
    #
    #output_df["p_val"] <- f_pval


     output_df["p_val"] <- summary(output_drc)[3]$coefficients[1,4]


     }else{

    output_df <- NA

     }

  if(saveModel==T){
    model = output_drc
    return(list(model,output_df))

  }else{
    return(output_df)
  }
}


tdsR_get_params <- function(inputData,
                            timeTreatment,
                            upperLimit,
                            upperLimitThreshold,
                            orderConc,
                            saveModel){

  max_k <- NA

  time <- list()

  time$points <- as.numeric(as.character(unique(inputData$time)))

  time$max <- max(time$points)

  time$min <- min(time$points)

  params <- matrix(data = NA, ncol = 5, nrow = length(time$points))

  #params <- as.data.frame(params)

  colnames(params) <- c("h", "l_asymp","u_assymp", "coef_e", "p_val")

  rownames(params) <- time$points

  time$limits <- setNames(rep(0,
                              times = length(time$points)),
                          time$points) # limit to search

  time$prior <-  rep(1,length(time$points)) / length(time$points)  #uniform prior

  time$prior <- setNames(time$prior, time$points)

  time$posterior <- setNames(rep(0,
                                 times = length(time$points)),
                             time$points) # limit to search

  # now call drc and fill parameters start timepoint and end time point,
  #TODO params[rownames(params) == c(time$min, time$max),]

  tmp.time <- inputData[inputData$time == time$max,]

  conc <- tmp.time$concentration

  fc_ttm <- tmp.time$fc_ttm

  if(orderConc == T){conc = conc[order(fc_ttm, decreasing = T)]}

  tmp.time$concentration <- conc

  params[rownames(params) == time$max,] <-  try(tdsR_logistic_fit(inputData = tmp.time,
                                                                  upperLimit = upperLimitThreshold, saveModel = F),silent = T)

  if(params[rownames(params) == time$max,2] >= upperLimitThreshold | is.na(params[rownames(params) == time$max,2]) |
     params[rownames(params) == time$max,2] >= upperLimit){

    params[rownames(params) == time$max,2] <- upperLimit

  }else{

    lapply(time$points, function(time_point){

      #time_point <- time$max

      inputData =  inputData[inputData$time == time_point,]

      conc <- inputData$concentration

      fc_ttm <- inputData$fc_ttm

      if(orderConc == T){conc = conc[order(fc_ttm, decreasing = T)]}

      inputData$concentration <- conc

      output <- try(tdsR_logistic_fit(inputData =  inputData,
                                      upperLimit = upperLimit,
                                      saveModel = saveModel),
                    silent = T)

    }) -> params

    if(saveModel==T){

      lapply(params, function(x){
        #x= params[5]
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

  ##### checking when model crashes ####

  return.l_assymp <- params[,2] == upperLimit

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

  #if(any(is.na(params[,"l_asymp:(Intercept)"]))){

  #time$return <- apply(params, 1, function(i) all(is.na(i)))

  # time$return <- time$return[time$return == T]
  #
  # time$return <- time$return[names(time$return) == max(as.numeric(names(time$return)))]
  #
  # time$return <- names(time$return)
  #
  # }else{
  #
  #   #diff(params[,"l_asymp:(Intercept)"])  #which(abs(params[,"l_asymp:(Intercept)"]-1)==min(abs(params[,"l_asymp:(Intercept)"]-1)))
  #
  #   time$return <- as.numeric(names(which(diff(params[,"l_asymp:(Intercept)"]) == min(diff(params[,"l_asymp:(Intercept)"])))))
  #
  # }



    # if(all(time$posterior == 0)){  #posterior is empty, run from prior
    #
    #    time$sample <- as.numeric(names(sample(x = time$prior,
    #                                size = 1,
    #                                replace = T,
    #                                prob = time$prior)))
    #
    #
    #    tmp.time <- inputData[inputData$time == time$sample,]
    #
    #    params[rownames(params) == time$sample,] <-  tdsR_logistic_fit(tmp.time)
    #
    #    if(){ #if sampled within optimal window,
    #
    #    }else{ # if time if na, shorten the window fro
    #
    #
    #    }
    #
    #
    #
    # }else{ #run and update posterior
    #
    # }

  return(list(params, estimated_onset = time$return, models))


}


tdsR_smooth <- function(inputData, groupingVariables){


  groupingVariables <- append(groupingVariables, "concentration")

  groups <- do.call(paste, inputData[,groupingVariables])

  lapply(split(inputData, groups), function(x){

    #print(x$cell_line[1])
    #print(x$agent[1])
    #x = split(inputData, groups)[[1]]

    out <- x[!duplicated(x$time),]

    rownames(out) <- out$time

    model <- mgcv::gam(cell_count ~ s(time, bs = "cs"), data = x)

    timeInput <- data.frame(time = unique(x$time))

    out[order(match(rownames(out), timeInput$time)),"cell_count"] <- predict(model, timeInput)

    return(out)

    }) -> out

  out <- do.call(rbind, out)

  return(out)

}


tdsR_fit <- function(inputData, groupingVariables, timeTreatment = 0, upperLimit = 0.9,
                     upperLimitThreshold = 0.8, smoothData = T,orderConc = T,saveModel = F){

  #FIXME option for user to calculate trapezoid estimates

  #FIXME option for user to use growth rates (trapezoid) instead of growth curves

  if(smoothData == T){inputData = tdsR_smooth(inputData, groupingVariables)}

  inputData <- tdsR_convert(inputData = inputData, case = "C", initial_count = T)

  inputData$fc_ttm = with(inputData, cell_count/cell_count__ctrl)
  inputData$fc_ctr = with(inputData, cell_count__ctrl/cell_count__time0)

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

    tmp <- tdsR_get_params(inputData =subset_data, timeTreatment = timeTreatment,
                           upperLimit=upperLimit, upperLimitThreshold=upperLimitThreshold,
                           orderConc=orderConc,saveModel = saveModel)

    tmp[[1]] <- cbind(key, tmp[[1]])

    names(tmp[[2]]) <- key

    return(tmp)

  })

  params <- lapply(output, "[[",1)

  names(params) <- keys

  estimated_onset <- unlist(lapply(output, "[[",2))

  names(estimated_onset) <- keys

  if(saveModel==T){
    models = lapply(output, "[[",3)
    return(list(params, estimated_onset, models))
  }else{
    return(list(params, estimated_onset))
  }



  #tmp <- lapply(output, "[[",1)
  #names(tmp) <- keys

  #View(tmp[["HT29 Methotrexate"]])
  # create a conditional if endpoint doesnt produce a fit, cell is resistant (t_), if startpoint

  #then call drc again with a new time




  # sample endpoints, if there's no fit, return maximun resistance


  #FIXME how to correct for growth rate? cells that divide less tend to have later effects.
  #FIXME maybe adjust t_onset by k, normalize by k

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


