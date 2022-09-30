#' Get grdR output
#'
#' @param outputData output data from get_fit
#' @param metric return grdR or parameters
#'
#' @return
#' @export
#'
#' @examples
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


