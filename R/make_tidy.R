#'  make data tidy
#'
#'  @description  Make standard input data into tidy format/long format
#'
#' @param inputData input data.frame in standard format
#' @param type type of function to average data, eg. median, mean...
#'
#' @return
#'
#' @examples
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

    data_ttm <- get_average(data = data_ttm,type = type)

    data_ctrl <- inputData_ctrl[inputData_ctrl$grouping_cols==group,]

    data_ctrl <- get_average(data = data_ctrl,type = type)

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

