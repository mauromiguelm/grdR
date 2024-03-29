% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_GRD.R
\name{plot_GRD}
\alias{plot_GRD}
\title{Plot GRD results as bar plot}
\usage{
plot_GRD(inputData, groupingVariables)
}
\arguments{
\item{inputData}{GRD fitted data}

\item{groupingVariables}{variable to group the data}
}
\value{
a plot
}
\description{
Plot GRD results as bar plot
}
\examples{
output <- get_fit(sample_data, groupingVariables = "perturbation",
 smoothData = FALSE, upperLimitThreshold = 1,
 timeTreatment = 1,upperLimit = 1,orderConc = TRUE,saveModel = FALSE)

plot_GRD(inputData = output, groupingVariables = c("perturbation"))
}
