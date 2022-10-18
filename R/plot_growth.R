#' Plot to check GRD on growth curves
#'
#' @param inputData Input data in GRD format for a single pertuabtion
#' @param groupingVariables variables to group the perturbation
#' @param GRD fitted GRD to be plotted as vertical line
#' @param addLegend shoudl concentration legend be added to plot
#'
#' @return a plot object
#' @export
#'
#' @examples
#' inputData <- subset(sample_data,perturbation == "C")
#'
#' outputGRD <- get_fit(inputData = inputData,groupingVariables = "perturbation",
#'     smoothData = FALSE, upperLimitThreshold = 1,
#'     timeTreatment = 1,upperLimit = 1, orderConc = TRUE,saveModel = FALSE)
#'
#' plot_growth(inputData = inputData,groupingVariables = "perturbation", GRD = outputGRD$GRD)

plot_growth <- function(inputData,groupingVariables, GRD, addLegend = FALSE){

  requireNamespace("grDevices", quietly = TRUE)

  xmin <- min(inputData$time)
  xmax <- max(inputData$time)
  xticks <- seq(xmin,xmax, length.out = 3)

  control <- subset(inputData, concentration == 0)

  perturbation <- subset(inputData, concentration != 0)

  colfun <- grDevices::colorRampPalette(c("firebrick1","firebrick4"))

  colMap <- data.frame(colors = colfun(length(unique(perturbation$concentration))), conc_order = sort(unique(perturbation$concentration)))

  plot(control$time, control$cell_count , type = "l", col = "blue", xlab = "Time [Days]", ylab = "Cell Count",
       lwd = 3,xaxt="n")

  axis(side=1, at=xticks, labels = TRUE)
  invisible(
    lapply(unique(perturbation$concentration), function(conc){

      sub_data <- subset(perturbation, concentration == conc)
      lines(sub_data$time, sub_data$cell_count, col = colMap[colMap$conc_order==conc,'colors'], lwd = 2.5)
      })
  )
  lines(control$time, control$cell_count , type = "l", col = "blue", lwd = 3)
  if(exists("GRD")){abline(v = GRD)}

  if(addLegend==TRUE){
    legend("topleft", inset=.02, title="Concentration",
           legend = colMap$conc_order, fill=c("blue", colMap$colors), horiz=F, cex=0.5)
  }
}
