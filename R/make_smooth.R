#' Make data smooth
#' @description  Denoise data with smoothing function from mgcv::gam
#'
#' @param inputData input data.frame in standard format
#' @param groupingVariables variable to group (eg. controls, drugs, cells, etc..)
#'
#' @return
#'
#' @examples
make_smooth <- function(inputData, groupingVariables){

  groupingVariables <- append(groupingVariables, "concentration")

  groups <- do.call(paste, inputData[,groupingVariables])

  lapply(split(inputData, groups), function(x){

    out <- x[!duplicated(x$time),]

    rownames(out) <- out$time

    model <- mgcv::gam(cell_count ~ s(time, bs = "cs"), data = x)

    timeInput <- data.frame(time = unique(x$time))

    out[order(match(rownames(out), timeInput$time)),"cell_count"] <- stats::predict(model, timeInput)

    return(out)

    }) -> out

  out <- do.call(rbind, out)

  return(out)

}
