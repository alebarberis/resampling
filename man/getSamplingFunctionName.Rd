% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0-utility-functions.R
\name{getSamplingFunctionName}
\alias{getSamplingFunctionName}
\title{Sampling Function Name}
\usage{
getSamplingFunctionName(
  id = c("rswor", "srswor", "stratified_rswor", "balanced_rswor", "permutation",
    "kfolds", "stratified_kfolds", "balanced_kfolds", "leave_p_out", "leave_one_out",
    "rswr", "srswr", "stratified_rswr", "balanced_rswr", "bootstrap")
)
}
\value{
A character string, the name of the random sampling function.
}
\description{
This function returns the name of the sampling function in input.
}
\author{
Alessandro Barberis
}
\keyword{internal}
