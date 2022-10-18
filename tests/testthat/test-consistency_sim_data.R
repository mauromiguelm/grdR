library(grdR)
library(unittest, quietly = TRUE)

if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })

grd_output <- grdR::get_fit(sample_data, groupingVariables = "agent",
                            smoothData = FALSE, upperLimitThreshold = 1,
                            timeTreatment = 1,upperLimit = 1,orderConc = TRUE,saveModel = F)

real <- grd_output$t_onset

estimation <- grd_output$GRD

unittest::ok(unittest::ut_cmp_equal(cor(real, estimation)>0.95, TRUE) ,"consistency in example data")
