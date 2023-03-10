% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/3-resampling-class.R
\name{validateResampling}
\alias{validateResampling}
\title{Validator of \code{resampling} Objects}
\usage{
validateResampling(x)
}
\arguments{
\item{x}{a \code{\link{resampling}} object}
}
\value{
An object of class \code{resampling}.
}
\description{
This is the validator of the \code{resampling} objects.
}
\examples{
\dontrun{
#No error is raised
validateResampling(newResampling())

#An error is raised
validateResampling(
newResampling(
 method = 'random',
 N = 3,
 samples = list(c(1, 4))
)
)
}
}
\author{
Alessandro Barberis
}
\keyword{internal}
