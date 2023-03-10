% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/3-resampling-class.R
\name{as.data.frame.resampling}
\alias{as.data.frame.resampling}
\title{Coerce to a data frame}
\usage{
\method{as.data.frame}{resampling}(x, ..., stringsAsFactors = FALSE)
}
\arguments{
\item{x}{an object of class \code{\link{resampling}}}

\item{...}{additional arguments to \code{\link{data.frame}}}

\item{stringsAsFactors}{logical: should character vectors be converted
    to factors?  The \sQuote{factory-fresh} default has been \code{TRUE}
    previously but has been changed to \code{FALSE} for \R 4.0.0.}
}
\value{
A data frame.
}
\description{
Function to coerce a \code{\link{resampling}} object to a data frame.
}
\examples{
#resampling object
x = resampling(
 method = 'rswor',
 N = 10,
 samples = list(c(1,5,7), c(6,2,9))
)

#print
as.data.frame(x)

}
\author{
Alessandro Barberis
}
