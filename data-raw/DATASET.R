
# #test data script -------------------------------------------------------

#define params

k = 0.3
t_ttm <- 1
nreps <- 5
time <- seq(0,3,0.1)
conc <- matrix(base::rep(c(0,seq(0.5,8,length.out = 6)),  each = nreps), dimnames = list(NULL, "concentration"))

params <- matrix(data = c(runif(nreps, min = 2, max = 2), seq(1,1.8,length.out = 5)), dimnames = list(NULL, c("halfeff", "t_onset")), nrow = nreps, ncol = 2)
params <- cbind(do.call("rbind", rep(list(params), 7)), conc)
params <- do.call(rbind, replicate(length(time), params, simplify=FALSE)) #replicate matrix to acommodate time
params <- cbind(params, time = rep(time, times = nreps))

#generate simulated data

sapply(1:dim(params)[1], function(x)

  model_growth(cell_tz = 5, t = params[x,'time'], t_onset = params[x,"t_onset"], k =  k, maxeff = 1.0, halfeff = params[x,"halfeff"], conc = params[x,"concentration"], hill = 1.6)

) -> sample_data

sample_data <- cbind(params, output = sample_data)

sample_data <- reshape2::dcast(data.frame(sample_data),halfeff + t_onset +concentration ~time, value.var="output")

# prepare data for input in GRDR

tidyr::gather(data.frame(sample_data, check.names = F), key = "time", value = "cell_count", -c("halfeff", "t_onset", "concentration")) -> sample_data

sample_data <- cbind(data.frame(agent = paste(sample_data$halfeff, sample_data$t_onset, sep = "_")), sample_data)

sample_data$k = k

usethis::use_data(sample_data, overwrite = TRUE)
