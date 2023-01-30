#'@include 3-resampling-class.R
NULL

#'Resample
#'
#'@description Handler function providing easy access and uniform syntax to the
#'various implemented repeated sampling techniques.
#'See the **Details** section below for further information.
#'
#'@param x either an integer representing the population size, or a vector of
#'stratification variables
#'@param n integer, either the sample size or the number of elements to holdout
#'@param k integer, the number of repeated samples to generate. Used as the
#'number of folds in k-fold sampling
#'@param method character string, one of the supported sampling techniques
#'@param prob (optional) vector of positive numeric values, the
#'probability weights for obtaining the elements from the population.
#'If provided, its length must match the population size
#'@param undersample logical, whether to remove elements from the population
#'in order to try to obtain balanced data
#'
#'@return An object of class [`resampling`].
#'
#'@details This function internally calls one of the implemented functions for
#'repeated sampling based on the value of the `method` argument:
#'
#'\describe{
#' \item{\code{"rswor"               }}{\code{\link{repeatedSampleWithoutReplacement}}}
#' \item{\code{"srswor"              }}{\code{\link{repeatedSimpleRandomSampleWithoutReplacement}}}
#' \item{\code{"stratified_rswor"    }}{\code{\link{repeatedStratifiedSampleWithoutReplacement}}}
#' \item{\code{"balanced_rswor"      }}{\code{\link{repeatedBalancedSampleWithoutReplacement}}}
#' \item{\code{"permutation"         }}{\code{\link{repeatedPermutationSample}}}
#' \item{\code{"kfolds"              }}{\code{\link{repeatedRandomKm1Folds}}}
#' \item{\code{"stratified_kfolds"   }}{\code{\link{repeatedStratifiedKm1Folds}}}
#' \item{\code{"balanced_kfolds"     }}{\code{\link{repeatedBalancedKm1Folds}}}
#' \item{\code{"leave_p_out"         }}{\code{\link{repeatedLeavePOut}}}
#' \item{\code{"leave_one_out"       }}{\code{\link{repeatedLeaveOneOut}}}
#' \item{\code{"rswr"                }}{\code{\link{repeatedSampleWithReplacement}}}
#' \item{\code{"srswr"               }}{\code{\link{repeatedSimpleRandomSampleWithReplacement}}}
#' \item{\code{"stratified_rswr"     }}{\code{\link{repeatedStratifiedSampleWithReplacement}}}
#' \item{\code{"balanced_rswr"       }}{\code{\link{repeatedBalancedSampleWithReplacement}}}
#' \item{\code{"bootstrap"           }}{\code{\link{repeatedBootstrapSample}}}
#'}
#'
#'See the specific function help page for further details on the implemented
#'methodology.
#'
#'@author Alessandro Barberis
#'
#'@examples
#'#Set seed for reproducibility
#'set.seed(seed = 5381L)
#'
#'#Random sampling without replacement
#'resample(
#'  x = 10,
#'  n = 5,
#'  k = 2,
#'  method = "rswor"
#')
#'
#'#Simple random sampling without replacement
#'resample(
#'  x = 10,
#'  n = 5,
#'  k = 2,
#'  method = "srswor"
#')
#'
#'#Permutation
#'resample(
#'  x = 10,
#'  k = 2,
#'  method = "permutation"
#')
#'
#'#Stratified random sampling without replacement
#'resample(
#'  x = c(rep("a", 3),rep("b", 6)),
#'  n = 6,
#'  k = 2,
#'  method = "stratified_rswor"
#')
#'
#'
#'#Balanced random sampling without replacement
#'resample(
#'  x = c(rep("a", 3),rep("b", 6)),
#'  n = 6,
#'  k = 2,
#'  method = "balanced_rswor"
#')
#'
#'#K-folds sampling
#'resample(
#'  x = 10,
#'  k = 3,
#'  method = "kfolds"
#')
#'
#'#Stratified k-folds sampling
#'resample(
#'  x = c(rep("a", 3),rep("b", 6)),
#'  k = 3,
#'  method = "stratified_kfolds"
#')
#'
#'#Balanced k-folds sampling (balanced population)
#'resample(
#'  x = c(rep(1,6),rep(2,6)),
#'  k = 3,
#'  method = "balanced_kfolds"
#')
#'
#'#Balanced k-folds sampling (unbalanced population)
#'resample(
#'  x = c(rep(1,6),rep(2,12)),
#'  k = 3,
#'  method = "balanced_kfolds",
#'  undersample = TRUE
#')
#'
#'#Leave-p-out sampling
#'resample(
#'  x = 5,
#'  n = 2,
#'  method = "leave_p_out"
#')
#'
#'#Leave-one-out sampling
#'resample(
#'  x = 5,
#'  method = "leave_one_out"
#')
#'
#'#Random sampling with replacement
#'resample(
#'  x = 10,
#'  n = 5,
#'  k = 2,
#'  method = "rswr"
#')
#'
#'#Simple random sampling with replacement
#'resample(
#'  x = 10,
#'  n = 5,
#'  k = 2,
#'  method = "srswr"
#')
#'
#'#Stratified random sampling with replacement
#'resample(
#'  x = c(rep("a", 3),rep("b", 6)),
#'  n = 6,
#'  k = 2,
#'  method = "stratified_rswr"
#')
#'
#'
#'#Balanced random sampling with replacement
#'resample(
#'  x = c(rep("a", 3),rep("b", 6)),
#'  n = 6,
#'  k = 2,
#'  method = "balanced_rswr"
#')
#'
#'#Bootstrap sampling
#'resample(
#'  x = 10,
#'  k = 2,
#'  method = "bootstrap"
#')
#'
#'@export
resample <- function(
    x,
    n,
    k,
    method = c(
      "rswor",
      "srswor",
      "stratified_rswor",
      "balanced_rswor",
      "permutation",
      "kfolds",
      "stratified_kfolds",
      "balanced_kfolds",
      "leave_p_out",
      "leave_one_out",
      "rswr",
      "srswr",
      "stratified_rswr",
      "balanced_rswr",
      "bootstrap"
    ),
    prob = NULL,
    undersample = FALSE){

  #match
  method = match.arg(method)

  #check x
  if(isTRUE(is.vector(x))){
    len = length(x)
    if(isTRUE(len==1L)){
      N = x
      strata = NULL
    } else if(isTRUE(len>1)){
      strata = x
      N = length(strata)
    } else {
      stop("'x' should be either a length-1 vector providing the population size or a vector of stratification variables.")
    }
  }

  #define args
  args = as.list(environment())

  #update args
  args = c(
    args,
    list(
      p = args$n
    )
  )

  #get resampling function
  resampleFun = getResamplingFunction(id = method)

  #match formals
  args = args[names(formals(fun = resampleFun))]

  #compute
  out = do.call(what = resampleFun, args = args)

  #create output object
  out = resampling(
    method = method,
    N = N,
    removed = if(!is.null(attr(out, "removed.data"))){
      attr(out, "removed.data")
    } else {integer()},
    samples = out
  )

  #return
  return(out)
}
