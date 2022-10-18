#' Plot GRD results as bar plot
#'
#' @param inputData GRD fitted data
#' @param groupingVariables variable to group the data
#'
#' @return a plot
#' @export
#'
#' @examples
#' output <- get_fit(sample_data, groupingVariables = "perturbation",
#'  smoothData = FALSE, upperLimitThreshold = 1,
#'  timeTreatment = 1,upperLimit = 1,orderConc = TRUE,saveModel = FALSE)
#'
#' plot_GRD(inputData = output, groupingVariables = c("perturbation"))

plot_GRD <- function(inputData, groupingVariables){

  if(length(groupingVariables)>1){
    groupedVariables <- apply(inputData[,groupingVariables],1, paste,collapse = " ")
  }else{
    groupedVariables <- inputData[,groupingVariables]
  }


  #adjust margins
  par(mar=c(10,4,4,4))

  barplot(inputData$GRD , border=F ,
          names.arg=groupedVariables,
          las=2,
          ylim = c(0, ceiling(max(inputData$GRD))),
          main="",
          ylab = "GRD")
}
