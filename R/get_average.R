#' get_average
#' @description Average the data based on groups provided within data.frame
#' @param
#' @param data standard data format input
#' @param type type of function to average data, eg. median, mean...
#'
#' @return
#'
#' @examples
get_average = function(data, type) {
  stats::aggregate(cell_count~.,data=data, get(type), na.rm = T)
}
