% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/2-resampling-functions.R
\name{repeatedBalancedKm1Folds}
\alias{repeatedBalancedKm1Folds}
\title{Repeated k-1 Balanced Folds}
\usage{
repeatedBalancedKm1Folds(k, strata, undersample = FALSE, prob = NULL)
}
\arguments{
\item{k}{number of folds}

\item{strata}{vector of stratification variables. The population size is
\code{length(strata)}}

\item{undersample}{logical, whether to remove elements from the population
in order to try to obtain balanced folds}

\item{prob}{(optional) vector of positive numeric values, the probability weights
for obtaining the \code{strata} elements. If provided, it must be the same
length as \code{strata}}
}
\value{
A list of length \code{k} where each element is a vector
containing the indices of the sampled data.
}
\description{
Takes repeated balanced samples from the population.
See the \strong{Details} section below for further information.
}
\details{
Each element in the population is randomly assigned to one of the k
folds so that the percentage of each stratum in the population is balanced
in each fold (see \code{\link{balancedKFolds}} function for further details).
A list of length k is then created from these folds, so that the i-th
item of the list is a vector of indices generated by removing the i-th fold
and merging the remaining k - 1 folds together.
}
\examples{
#Set seed for reproducibility
set.seed(seed = 5381L)

#Define balanced strata
strata = c(rep(1,6),rep(2,6))

#Check ratio
table(strata)/length(strata)

#Assign data to 3 folds
i = repeatedBalancedKm1Folds(
 strata = strata,
 k = 3
)
#Check indices
i
#Check ratio in the samples made of k-1 folds
table(strata[i[[1]]])/length(strata[i[[1]]])
table(strata[i[[2]]])/length(strata[i[[2]]])
table(strata[i[[3]]])/length(strata[i[[3]]])

#Define unbalanced strata
strata = c(rep(1,6),rep(2,12))

#Check ratio
table(strata)/length(strata)

#Assign data to 3 folds
i = repeatedBalancedKm1Folds(
 strata = strata,
 k = 3,
 undersample = TRUE
)
#Check folds
i
#Check ratio in the samples made of k-1 folds
table(strata[i[[1]]])/length(strata[i[[1]]])
table(strata[i[[2]]])/length(strata[i[[2]]])
table(strata[i[[3]]])/length(strata[i[[3]]])

}
\seealso{
\code{\link{balancedKFolds}}
}
\author{
Alessandro Barberis
}
