# library(ggplot2)
# library(tidyr)
# library(grDevices)
#
#
# model_growth <- function(cell_tz, t, t_onset, k, maxeff, halfeff, conc, hill){
#
#   maxeff <- ifelse(t_onset > t, 0, maxeff)
#
#   if(t >= t_onset){
#
#     cell_tz <- cell_tz * exp( ( t_onset * k ) )
#
#     t <- t - t_onset
#
#     return( cell_tz * exp( ( t * k *( 1 - ( (maxeff*conc ** hill )/ (halfeff ** hill + conc ** hill) ) ) ) ) )
#
#   }else{
#
#     return( cell_tz * exp( ( t * k *( 1 - ( (maxeff*conc ** hill )/ (halfeff ** hill  + conc ** hill) ) ) ) ) )
#
#   }
# }
#
# colfun <- grDevices::colorRampPalette(c("firebrick1","firebrick4"))
#
# time <- seq(0,3,0.01)
#
# conc <- seq(0.5,8,length.out = 6)
#
# t_onset <- 0.52
#
# half_eff <- 7.28
#
# matrix_data <- sapply(time, function(xtime) model_growth(cell_tz = 5,t = xtime, t_onset = Inf, k =  0.3, maxeff = 1.0, halfeff = half_eff, conc = 1.5, hill = 1.6))
#
# matrix_data <- base::rbind(matrix_data,
#                            sapply(time, function(xtime) model_growth(cell_tz = 5,t = xtime, t_onset = t_onset, k =  0.3, maxeff = 1.0, halfeff = half_eff, conc = conc, hill = 1.6)))
#
#
# colnames(matrix_data) <- time
#
# matrix_data <- t(matrix_data)
#
# plot(rownames(matrix_data), matrix_data[,1] , type = "l", col = "blue", main = "Dynamic Growth Response", xlab = "Time [Days]", ylab = "Cell Count",
#      lwd = 3)
#
# abline(v = t_onset, col = "orange", lwd = 3)
#
# invisible(
#   lapply(2:ncol(matrix_data), function (col) lines(rownames(matrix_data), matrix_data[,col], col = colfun(ncol(matrix_data))[col], lwd = 2.5))
# )
# lines(rownames(matrix_data), matrix_data[,1] , type = "l", col = "blue", lwd = 3)
# legend(as.numeric(max(rownames(matrix_data)))/20, max(matrix_data),
#        legend = c("Control", paste("c = ", conc)), col = c("blue", colfun(ncol(matrix_data)-1)), lty = 1, lwd = 3,
#        box.lty = 0)
#
# legend(as.numeric(max(rownames(matrix_data)))/20, max(matrix_data)/1.4,
#        legend = c(paste0("half effect = ", half_eff)), col = c("orange"), lty = 1, lwd = 3,
#        box.lty = 0)
#
#
# #TODO test GR50, IC50 and GI50 for the previous data
#
# # sample the space of growth rates from 1.9 to 3.9, and run GRmetrics to compare GI50 with t_onset  --------
#
# # generate new data again
#
# time <- seq(0,3,0.1)
#
# t_ttm <- 1
#
# t_GRinterest <- c(0,1,3)
#
# conc <- matrix(base::rep(c(0,seq(0.5,8,length.out = 6)),  each = 100), dimnames = list(NULL, "concentration"))
#
# k = 0.3
#
# # params bellow sample k and t_onset
# #params <- matrix(data = c(runif(1000, min = 0.3, max = 1), runif(1000, min = 1.5, max = 2.5)), dimnames = list(NULL, c("k", "t_onset")), nrow = 1000, ncol = 2)
#
# # params bellow keep k constant and samples t_onset
#
# params <- matrix(data = c(runif(100, min = 1.1, max = 8), runif(100, min = 0.5, max = 2.5)), dimnames = list(NULL, c("halfeff", "t_onset")), nrow = 100, ncol = 2)
#
# params <- cbind(do.call("rbind", rep(list(params), 7)), conc)
#
# sapply(time, function(xtime)
#
#   model_growth(cell_tz = 5, t = xtime, t_onset = params[,"t_onset"], k =  k, maxeff = 1.0, halfeff = params[,"halfeff"], conc = params[,"concentration"], hill = 1.6)
#
#   #FIXME get a list of function args, to plot them later... they are evaluated in order, k[1] , t_onset[1]...k[2] , t_onset[2]  SOLVED, BUT CHECK website bellow
#   # https://stackoverflow.com/questions/18586758/how-to-evaluate-arguments-of-a-function-call-inside-other-function-in-r
#
#     ) -> matrix_data
#
# colnames(matrix_data) <- time
#
# matrix_data <- cbind(params, matrix_data)
#
# # create a function to estimate t_onset
#
# tidyr::gather(data.frame(matrix_data, check.names = F), key = "time", value = "cell_count", -c("halfeff", "t_onset", "concentration")) -> matrix_data
#
# matrix_data <- cbind(data.frame(agent = paste(matrix_data$halfeff, matrix_data$t_onset, sep = "_")), matrix_data)
#
# matrix_data <- subset(matrix_data, agent %in% unique(matrix_data$agent)[1:50])
#
# output_t <- list()
#
# output_t$keys <- unique(matrix_data$agent)
#
# output_t$t_onset <- (matrix_data$t_onset)
#
# output_t$half_effect <- (matrix_data$halfeff)
#
#
# output <- lapply(as.character(output_t$keys), function(key){
#
#   #key = as.character(output_t$keys)[1]
#
#   inputData = subset(matrix_data, as.character(agent) == key)
#
#   tdsR_fit(inputData, groupingVariables = "agent", smoothData = F, upperLimit = 0.9)
#
# })
#
# output_t$estimate <- as.numeric(do.call(rbind, lapply(output, "[[",2)))
#
# output_t$params <- (do.call(rbind, lapply(output, "[[",1)))
#
# plot(output_t$estimate, unique(output_t$t_onset))
#
# #FIXME CONTINUE HERE
# #FIXME CONTINUE HERE
# #FIXME CONTINUE HERE
# #FIXME CONTINUE HERE
# #FIXME CONTINUE HERE
# #FIXME CONTINUE HERE
#
# # calculate GR50 based on GRmetrics
#
# output_t$df_figs <- data.frame(cbind(output_t$estimate, unique(output_t$t_onset), unique(output_t$half_effect)))
#
# colnames(output_t$df_figs) <- c("estimate", "t_onset", "half_effect")
#
# output_t$df_figs$Nsampling <- 1:dim(output_t$df_figs)[1]
#
# output_t$df_figs <- output_t$df_figs[1:50,]
#
# output_t$df_figs$estimate <- scale(output_t$df_figs$estimate)
#
# output_t$df_figs$t_onset <- scale(output_t$df_figs$t_onset)
#
# output_t$df_figs$half_effect <- scale(output_t$df_figs$half_effect)
#
# output_t$df_figs$actual_t <- output_t$df_figs$t_onset
#
# output_t$df_figs <- tidyr::gather(output_t$df_figs,
#                                   key = "Parameter",
#                                   value = "output",
#                                   -c(Nsampling, actual_t))
#
# output_t$df_figs$Parameter <- factor(output_t$df_figs$Parameter,
#                                      levels = c("estimate", "t_onset", "half_effect"),
#                                      labels = c("T estimate","T onset", "Half Effect"))
#
# library(RColorBrewer)
#
# ggplot(data = output_t$df_figs, aes(x = Parameter, y = output, group = Nsampling)) +
#   geom_line(aes(alpha = 0.1, color = (actual_t)), size = 2)+
#   scale_color_continuous(low = "#810f7c", high = "#006d2c") +
#   scale_x_discrete(position = "top")+
#   theme_bw()+
#   theme(legend.position = "none")+
#   theme(panel.border     = element_blank())+
#   theme(axis.title.y     = element_blank()) +
#   theme(axis.text.y      = element_blank()) +
#   theme(panel.grid.major.y = element_blank()) +
#   theme(panel.grid.minor.y = element_blank())+
#   theme(axis.title.x     = element_blank()) +
#   theme(panel.grid.major.x = element_blank()) +
#   theme(axis.text.x.top      = element_text(size=19))+
#   theme(axis.ticks       = element_blank()) +
#   theme(plot.title       = element_text(size=22, face = "bold", hjust = 0.5)) +
#   theme(plot.subtitle    = element_text(hjust = 0.5)) +
#   labs(
#     title = "Correlation of parameters and tdsR output"
#   )
#
#
#
#
# # ggplot(output_t$df_figs[1:50,], aes(x = 1, xend = 2, y =  estimate, yend = t_onset))+
# #   geom_segment()+
# #   theme_minimal()+
# #   scale_x_continuous(breaks = c(1,2)) +
# #   ylim(0,(1.1*(max(df$`1952`, df$`1957`))))  # X and Y axis limits
# #
# # ggplot(output_t$df_figs[1:50,], aes(x = 1, xend = 2, y =  estimate, yend = half_effect))+
# #   geom_segment()+
# #   theme_minimal()
#
#
# lapply(unique(matrix_data$agent), function(agent){
#
#   tmp <- matrix_data[matrix_data$agent == unique(matrix_data$agent)[agent] & matrix_data$time %in% t_GRinterest,]
#
#   tmp <- tmp[tmp$time != 0,] # replace t = 0 with t = t_ttm, as grmetrics require time at treatment
#
#   tmp$time <- ifelse(tmp$time == t_ttm, 0, tmp$time)
#
#   output1 = GRfit(inputData = tmp, groupingVariables =
#                     c('agent'), case = "C")
#
#   output1 <- GRmetrics::GRgetMetrics(output1)
#
#   return <- data.frame(halfeff = as.numeric(strsplit(output1$experiment, split = "_")[[1]][1]),
#                        t_onset = as.numeric(strsplit(output1$experiment, split = "_")[[1]][2]),
#                        output1[,6:21])
#
#   return(return)
#
# }) -> tmp
#
# tmp <- do.call(rbind, tmp)
#
# library(plotly)
#
# tmp_fig <- tmp
#
# tmp_fig <- tmp_fig[tmp_fig$t_onset<2,]
#
# tmp_fig$GR50 <- ifelse(tmp_fig$GR50 == Inf | tmp_fig$GR50 == -Inf, NA, tmp_fig$GR50)
#
# ggplot(tmp_fig, aes(x = (halfeff), y = t_onset, col = (GR50)))+
#   geom_point()+
#   scale_color_gradient(low="blue", high="red")
#
# plot_ly(x = tmp_fig$GR50, y = tmp_fig$halfeff, color = tmp_fig$t_onset,
#         mode = "markers", size = tmp_fig$GR50)
#
#
# ggplot(tmp_fig, aes(x = halfeff, y = t_onset, col = h_GR))+
#   geom_point()+
#   scale_color_gradient(low="blue", high="red")
#
# ggplot(tmp_fig, aes(x = halfeff, y = t_onset, col = log(IC50)))+
#   geom_point()+
#   scale_color_gradient(low="blue", high="red")
#
# ggplot(tmp_fig, aes(x = halfeff, y = t_onset, col = log(h)))+
#   geom_point()+
#   scale_color_gradient(low="blue", high="red")
#
# pl <- plot(tmp$t_onset, log10(log10(tmp$GR50)), type = "p",
#      xlab = "t_onset",
#      ylab = "log(GR50)")
#
# library(ggfortify)
#
# tmp.pca <-  (prcomp(x = t(tmp_fig[,c(1,2,4)]), center = T, scale = T))
#
# autoplot(tmp.pca, data = tmp_fig, colour = "GR50")
#
# autoplot(tmp.pca, data = tmp_fig, colour = "GR50",  loadings = TRUE, loadings.colour = 'blue',loadings.label = F, loadings.label.size = 10)
#
#
#
# #TODO estimate D, create a sampling model that will approach D, where we will get the best estimate and use less computer
