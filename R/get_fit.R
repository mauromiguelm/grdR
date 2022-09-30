#' Fit grdR on growth curves
#'
#' @param inputData  input data.frame in standard format
#' @param groupingVariables variable to group (eg. controls, drugs, cells, etc..)
#' @param timeTreatment time of intervention treatment
#' @param upperLimit upperl in DRC: numeric upper limit parameters in the model
#' @param upperLimitThreshold threshold to evaluate GRD effect after model fit
#' @param smoothData should function to smooth data be applied?
#' @param orderConc should concentration be ordered based on effect?
#' @param saveModel should model parameters be returned?
#'
#' @return
#' @export
#'
#' @examples
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


