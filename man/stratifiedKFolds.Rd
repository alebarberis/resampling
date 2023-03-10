% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/1-sampling-functions.R
\name{stratifiedKFolds}
\alias{stratifiedKFolds}
\title{Stratified K Folds}
\usage{
stratifiedKFolds(strata = NULL, k)
}
\arguments{
\item{strata}{vector of stratification variables. The population size is
\code{length(strata)}}

\item{k}{number of folds}
}
\value{
A vector of length \code{length(strata)} containing the fold ids.
}
\description{
Assigns the population to k stratified folds.
See the \strong{Details} section below for further information.
}
\details{
Each element in the population is assigned to one of the k folds so
that the percentage of each stratum in the population is preserved in each
fold.
}
\examples{
#Set seed for reproducibility
set.seed(seed = 5381L)

#Define strata
strata = c(1,1,1,2,2,2,2,2,2)

#Check ratio
table(strata)/length(strata)

#Assign data to 3 folds
i = stratifiedKFolds(
 strata = strata,
 k = 3
)
#Check folds
i
#Check ratio in the folds
table(strata[i==1])/length(strata[i==1])
table(strata[i==2])/length(strata[i==2])
table(strata[i==3])/length(strata[i==3])

}
\author{
Alessandro Barberis
}
