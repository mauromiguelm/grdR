#' Get grdR output
#'
#' @param outputData output data from get_fit
#' @param metric return grdR or parameters
#'
#' @return
#' @export
#'
#' @examples
#' grd_output <- get_fit(sample_data, groupingVariables = "agent",
#'  smoothData = F, upperLimitThreshold = 1,
#'  timeTreatment = 1,upperLimit = 1,orderConc = T,saveModel = F)
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


