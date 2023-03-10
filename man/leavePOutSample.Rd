% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/1-sampling-functions.R
\name{leavePOutSample}
\alias{leavePOutSample}
\title{Leave-P-Out Sampling}
\usage{
leavePOutSample(N, p, prob = NULL)
}
\arguments{
\item{N}{positive integer value, the population size}

\item{p}{integer, the number of elements to holdout}

\item{prob}{(optional) N-length vector of positive numeric values, the
probability weights for obtaining the \code{N} elements}
}
\value{
A vector of length \code{N-p} containing the index of the
computed random set of observations.
}
\description{
Takes a random sample of size \code{N-p} from the population.
See the \strong{Details} section below for further information.
}
\details{
A random sample of size \code{p} is taken from the population and used
as holdout data.
This function returns a sample of size \code{N-p} obtained by removing the holdout
sample from the population.
}
\examples{
#Set seed for reproducibility
set.seed(seed = 5381L)

#Take one sample leaving out p elements
leavePOutSample(N = 5, p = 2)

#Equivalent to leave-one-out
leavePOutSample(N = 5, p = 1)

}
\author{
Alessandro Barberis
}
