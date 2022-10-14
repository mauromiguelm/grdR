#' get_average
#' @description Average the data based on groups provided within data.frame
#' @param data standard data format input
#' @param type type of function to average data, eg. median, mean...
#'
#' @return averaged groups
get_average = function(data, type) {
  stats::aggregate(cell_count~.,data=data, get(type), na.rm = TRUE)
}
