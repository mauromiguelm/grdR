#' Get grdR output
#'
#' @param outputData output data from get_fit
#' @param metric return grdR or parameters
#'
#' @return output type from grd model
#' @export
#'
#' @examples
#' grd_output <- get_fit(sample_data, groupingVariables = "agent",
#'  smoothData = FALSE, upperLimitThreshold = 1,
#'  timeTreatment = 1,upperLimit = 1,orderConc = TRUE,saveModel = FALSE)
#'
#' get_output(outputData = grd_output, metric = "grdR")
#'
#' get_output(outputData = grd_output, metric = "parameters")

get_output <- function(outputData, metric){

  if(metric == "grdR"){

    out <- outputData[[2]]

    groups <- names(out)

    out <- data.frame(groups = names(out), grd = out)

  }else if(metric == "parameters"){

    out <- outputData[[1]]

    out <- do.call(rbind, out)

  }else{
    stop("metric not found")
  }

  return(out)

}


