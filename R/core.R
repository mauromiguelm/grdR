get_average = function(data, type) {
  aggregate(cell_count~.,data=data, get(type), na.rm = T)
}

make_tidy = function(inputData, type){

  expected_cols <- c('concentration','time','cell_count')

  if(!all(expected_cols %in%colnames(inputData))){
    #check if all necessary columns are present in the data
    missing_cols <- which(!expected_cols%in%colnames(inputData))
    stop(paste("Input data is incomplete, missing:", expected_cols[missing_cols]))
    }

  grouping_cols <- which(!colnames(inputData) %in% expected_cols)
  grouping_cols <- colnames(inputData)[grouping_cols]

  inputData_ttm <- inputData[inputData$time>0 & inputData$concentration!=0,]
  inputData_ttm$grouping_cols <- apply(inputData_ttm[,grouping_cols],1, paste, collapse = "_")
  inputData_ctrl <- inputData[inputData$concentration==0,]
  inputData_ctrl$grouping_cols <- apply(inputData_ctrl[,grouping_cols],1, paste, collapse = "_")

  lapply(unique(inputData_ttm$grouping_cols), function(group){

    #group = unique(inputData_ttm$grouping_cols)[1]

    data_ttm <- inputData_ttm[inputData_ttm$grouping_cols==group,]

    data_ttm <- get_average(data = data_ttm,type = "median")

    data_ctrl <- inputData_ctrl[inputData_ctrl$grouping_cols==group,]

    data_ctrl <- get_average(data = data_ctrl,type = "median")

    data_ctrl_t0 <- data_ctrl[data_ctrl$time==0,]
    data_ctrl_t0$concentration <- NULL
    data_ctrl_t0$time <- NULL
    data_ctrl$concentration <- NULL
    data_ctrl <- data_ctrl[data_ctrl$time>0,]
    colnames(data_ctrl_t0)[which(colnames(data_ctrl_t0)=="cell_count")] <- 'cell_count_t0'
    colnames(data_ctrl)[which(colnames(data_ctrl)=="cell_count")] <- 'cell_count_ctrl'

    #merge cells at time zero to treatment groups
    data_ttm <- merge(data_ttm, data_ctrl_t0)

    #merge match time control for each treatment group
    data_ttm <- merge(data_ttm, data_ctrl)

    data_ttm$grouping_cols <- NULL

    return(data_ttm)

  })-> output_data

  output_data <- do.call(rbind, output_data)
 return(output_data)

}

get_logistic_fit <- function(inputData, groupingVariables,
                              upperLimit, lowerLimit = 1E-10,
                              saveModel){

  if(!is.null(upperLimit)){upperLimit <- c(NA,NA, upperLimit, NA)}

  if(!is.null(lowerLimit)){lowerLimit <- c(NA,NA, lowerLimit, NA)}

  output_drc <- drc::drm(
    formula = as.numeric(fc_ttm) ~ as.numeric(concentration),
    na.action = na.omit,
    data = inputData,
    upperl = upperLimit,
    lowerl = lowerLimit,
    fct = drc::L.4())

  # test if logictic fit outperforms linear regression using ANOVA

  if(class(output_drc) == "drc" &&
     !is.null(stats::coef(output_drc)) &&
     !is.null(stats::residuals(output_drc))
  ) {

     output_df <- coef(output_drc)

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

  time$limits <- setNames(rep(0,
                              times = length(time$points)),
                          time$points)

  time$prior <-  rep(1,length(time$points)) / length(time$points)  #uniform prior

  time$prior <- setNames(time$prior, time$points)

  time$posterior <- setNames(rep(0,
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


make_smooth <- function(inputData, groupingVariables){


  groupingVariables <- append(groupingVariables, "concentration")

  groups <- do.call(paste, inputData[,groupingVariables])

  lapply(split(inputData, groups), function(x){

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


get_fit <- function(inputData, groupingVariables, timeTreatment = 0, upperLimit = 0.9,
                     upperLimitThreshold = 0.8, smoothData = T,orderConc = T,saveModel = F){

  if(smoothData == T){inputData = make_smooth(inputData, groupingVariables)}

  inputData <- make_tidy(inputData = inputData,type = 'median')

  inputData$fc_ttm = inputData$cell_count/inputData$cell_count_ctrl
  inputData$fc_ctr = inputData$cell_count_ctrl/inputData$cell_count_t0

  if(length(groupingVariables[!groupingVariables == "time"]) > 1){

    inputData$keys <- (base::do.call(paste, inputData[, groupingVariables[!groupingVariables == "time"]]))

  }else{

    inputData$keys <- inputData[, groupingVariables[!groupingVariables == "time"]]

  }

  keys <- unique(inputData$keys)

  output <- lapply(keys, function(key){

    print(key)

    subset_data <- subset(inputData, keys == key)

    tmp <- get_params(inputData =subset_data, timeTreatment = timeTreatment,
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
}


get_output <- function(inputData, metric){

  if(metric == "grdR"){

    out <- inputData[[2]]

    groups <- names(out)

    out <- data.frame(groups = names(out), grd = out)

  }else if(metric == "parameters"){

    out <- inputData[[1]]

    out <- do.call(rbind, out)

  }else{
    stop("metric not found")
  }

  return(out)

}


