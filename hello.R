#
# #library(drc)
#
# tdsR_arrange <- function(input, case = "C"){
#
#   #input = tmp
#   subset_data <- list()
#
#   if(case == "C"){
#
#     input_vars <- c("time", "cell_count", "concentration")
#
#     check_numeric <- sum(apply(input[,expected_vars],2, is.numeric))
#
#     if(check_numeric > 0) stop("Required columns time, cell_count
#                                and concentration not numeric")
#
#     # separate by concentration and time
#
#     subset_data$control_t0 <-     subset(input,
#                                          concentration == 0 & time == 0)
#
#     subset_data$control_tmatch <- subset(input,
#                                          concentration != 0 & time == 0)
#
#     subset_data$treated <-        subset(input,
#                                          concentration != 0 & time != 0)
#
#     list_names <- names(subset_data)
#
#      # create keys for individual time points and grouping variables
#
#     lapply(subset_data, function(data){
#
#       #data <- subset_data[[3]]
#
#       data[,] <- lapply(data[,!(colnames(data) %in% input_vars[-1])],
#                         as.character)
#
#       data[,"keys"] <- paste(data[,!(colnames(data) %in% input_vars[-1])],
#                              collapse = "_")
#
#       return(data)
#
#     }) -> subset_data
#
#     # subset_data$treated needs time zero and matching endpoint
#
#     subset_data$treated$treated$cell_count__ctrl = NA
#     subset_data$treated$cell_count__t0 = NA
#
#     apply(subset_data$treated[,'keys'],1,function(key){
#       return(key)
#     }) -> tmp
#
#
#
#     }if(case == "A"){
#
#         stop("Mauro should prepare case A") #FIXME
#
#     }
# }else{
#
#   stop("Mauro should write an error message") #FIXME
#
#   }
#
#
# tdsR_fit <- function(input){
#   # input is a matrix with columns (list col names) and concentrations
#
#   input = tmp
#
#   t_onset <- NULL
#
#   drc::drm()
#
#
# }
#
# t_onset <- NULL
#
#
