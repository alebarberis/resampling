#'@include 0-utility-functions.R
NULL

# Random Sampling --------------------------------------------------------------


#'Sample Input Validator
#'
#'@description This function checks the input provided to the random sampling
#'functions.
#'
#'@inheritParams stratifiedSample
#'
#'@return \code{TRUE} if the arguments are valid, or a string
#'containing the errors.
#'
#'@author Alessandro Barberis
#'
#'@examples
#'\dontrun{
#'#Valid input
#'isValidSampleInput(
#'  N=10,
#'  n=3
#')
#'
#'#Invalid input
#'isValidSampleInput(
#'  N=10,
#'  n=11,
#'  replace = FALSE
#')
#'}
#'
#'@keywords internal
isValidSampleInput <- function(
    N,
    n,
    prob,
    replace
) {
  errors <- character()

  if (isTRUE(missing(N))){
    errors = "Missing input: 'N' must be provided."
    return(errors)
  } else {
    if (isFALSE(is.numeric(N) & N > 0 & length(N)==1)){
      return("'N' must be a positive integer of length 1.")
    }
  }

  if (isTRUE(missing(n))){
    n = N
  } else {
    if (isFALSE(is.numeric(n) & n > 0 & length(n)==1)){
      return("'n' must be a positive integer of length 1.")
    }
  }

  if (isFALSE(!missing(replace) & is.logical(replace))){
    errors <- paste("'replace' is not a logical value: it must be TRUE or FALSE. ", sep = "")
    return(errors)
  }

  if (isTRUE(!missing(prob) && !is.null(prob))){
    if (isFALSE(is.numeric(prob) & all(prob >= 0))){
      msg <- paste("'prob' must contain positive numeric values. ", sep = "")
      errors <- c(errors, msg)
    }

    if (isFALSE(length(prob)==N)){
      msg <- paste("The vector of probability weights 'prob' is not of length 'N'. ", sep = "")
      errors <- c(errors, msg)
    }
  }

  if (isTRUE(n > N & !replace)) {
    msg <- paste("Sample size 'n' greater than population size 'N': this is not allowed when sampling without replacement. ", sep = "")
    errors <- c(errors, msg)
  }

  errors = if (isTRUE(length(errors) == 0)) TRUE else errors

  return(errors)
}

#'Sample Input Validator
#'
#'@description This function checks the input provided to the random sampling
#'functions.
#'
#'@inheritParams stratifiedSample
#'
#'@return \code{TRUE} if the arguments are valid, or a string
#'containing the errors.
#'
#'@author Alessandro Barberis
#'
#'@examples
#'\dontrun{
#'#Valid input
#'isValidStratifiedSampleInput(
#'  strata=c(1,1,1,2,2,2,2,2,2),
#'  n=3
#')
#'
#'#Invalid input
#'isValidStratifiedSampleInput(
#'  n=11,
#'  replace = FALSE
#')
#'}
#'@keywords internal
isValidStratifiedSampleInput <- function(
    strata,
    n,
    prob,
    replace
) {
  errors <- character()

  if (isTRUE(missing(strata))){
    errors = "Missing input: 'strata' must be provided."
    return(errors)
  } else {
    if (isFALSE(is.vector(strata) & length(strata)>0)){
      return("Invalid input: 'strata' must be a vector.")
    }
    N = length(strata)
  }


  if (isTRUE(missing(n))){
    n = N
  } else {
    if (isFALSE(is.numeric(n) & n > 0 & length(n)==1)){
      return("Invalid input: 'n' must be a positive integer of length 1.")
    }
  }

  if (isFALSE(!missing(replace) & is.logical(replace))){
    errors <- paste("'replace' is not a logical value: it must be TRUE or FALSE. ", sep = "")
    return(errors)
  }

  if (isTRUE(!missing(prob) && !is.null(prob))){
    if (isFALSE(is.numeric(prob) & all(prob >= 0))){
      msg <- paste("'prob' must contain positive numeric values. ", sep = "")
      errors <- c(errors, msg)
    }

    if (isFALSE(length(prob)==N)){
      msg <- paste("The vector of probability weights 'prob' is not matching the length of 'strata'. ", sep = "")
      errors <- c(errors, msg)
    }
  }

  if (isTRUE(n > N & !replace)) {
    msg <- paste("Sample size 'n' greater than population size: this is not allowed when sampling without replacement. ", sep = "")
    errors <- c(errors, msg)
  }

  errors = if (isTRUE(length(errors) == 0)) TRUE else errors

  return(errors)
}

#'Simple Random Samples
#'
#'@description Takes a simple random sample from the population.
#'See the **Details** section below for further information.
#'
#'@param N positive integer value, the population size
#'@param n positive integer value, the sample size
#'@param replace logical, whether to sample with replacement.
#'Default is \code{FALSE}
#'@param prob (optional) N-length vector of positive numeric values, the
#'probability weights for obtaining the \code{N} elements
#'
#'@return A vector of length \code{n} containing the index of the
#'computed random set of observations.
#'
#'@details This function is a wrapper to the \code{\link[base]{sample.int}}
#'function.
#'
#'@author Alessandro Barberis
#'
#'@examples \dontrun{
#'#Set seed for reproducibility
#'set.seed(seed = 5381L)
#'
#'#Simple random sample without replacement
#'randomSample(N = 10, n = 5, replace = FALSE)
#'
#'#Simple random sample with replacement
#'randomSample(N = 10, n = 5, replace = TRUE)
#'}
#'@keywords internal
randomSample <- function(
    N,
    n,
    replace = FALSE,
    prob = NULL
){
  #check input
  isValid = isValidSampleInput(N = N, n = n, replace = replace)
  if(isFALSE(isTRUE(isValid))){
    stop(paste(isValid, collapse = "\n"))
  }
  #Select n random samples from the total observations
  out = sample.int(n = N, size = n, replace = replace, prob = prob);
  #return
  return(out)
}

#'Stratified Random Samples
#'
#'@description Takes a stratified sample with or without replacement from the
#'population.
#'See the **Details** section below for further information.
#'
#'@param strata vector of stratification variables. The population size is
#'\code{length(strata)}
#'@param prob (optional) vector of positive numeric values, the probability weights
#'for obtaining the \code{strata} elements. If provided, it must be the same
#'length as \code{strata}
#'@inheritParams randomSample
#'
#'@return A vector of length \code{n} containing the index of the
#'computed random set of observations.
#'
#'@details Stratified sampling is a technique of sampling from a population that
#'can be partitioned into 'strata' (or 'subpopulations'), where each element in
#'the population is part of one and only one stratum. It is used to ensure that
#'subgroups of the population are represented in the taken sample.
#'This function implements the so-called "proportionate allocation", in which
#'the proportion of the strata in the population is maintained in the samples.
#'
#'@author Alessandro Barberis
#'
#'@references https://en.wikipedia.org/wiki/Stratified_sampling
#'
#'@examples \dontrun{
#'#Set seed for reproducibility
#'set.seed(seed = 5381L)
#'
#'#Define strata
#'strata = c(rep("a", 3),rep("b", 6))
#'
#'#Check ratio
#'table(strata)/length(strata)
#'
#'#Stratified random sample without replacement
#'i = stratifiedSample(
#'   strata = strata,
#'   n = 3,
#'   replace = FALSE
#')
#'#Check ratio in the sample
#'table(strata[i])/length(strata[i])
#'
#'#Stratified random sample with replacement
#'i = stratifiedSample(
#'   strata = strata,
#'   n = 3,
#'   replace = TRUE
#')
#'#Check ratio in the sample
#'table(strata[i])/length(strata[i])
#'}
#'@keywords internal
stratifiedSample <- function(
    strata,
    n,
    replace = FALSE,
    prob = NULL
){

  #check input
  isValid = isValidStratifiedSampleInput(strata = strata, n = n, replace = replace)
  if(isFALSE(isTRUE(isValid))){
    stop(paste(isValid, collapse = "\n"))
  }

  #population size
  N = length(strata)

  #cross tabulation
  xtable = table(strata)

  #Get the minimum number of observations per class (element != 0)
  min.nobs.class = min(xtable)

  #Get the strata names
  stratanames = names(xtable)

  #get n of levels
  n.strata = length(stratanames)

  #Create an empty vector
  out = NULL;

  #Loop over the strata to assign the
  #samples trying to keep the original strata frequency
  for(i in 1L:n.strata) {
    #Get the index of elements of current stratum
    el.index = which(strata==stratanames[i])
    #get number of element we want
    n.per.stratum = round(n * (xtable[stratanames[i]]/N))
    #Randomly select the element we want per stratum
    index = base::sample(x = el.index, size = n.per.stratum, replace = replace, prob = prob[el.index])
    #update index vector
    out = c(out, index);
  }
  #force to have length n
  out = out[1:n]

  #permutation
  out = out[sample.int(n = n, size = n, replace = F)]

  #return
  return(out)
}

#'Balanced Random Samples
#'
#'@description Takes a balanced sample with or without replacement from the
#'population.
#'See the **Details** section below for further information.
#'
#'@inheritParams stratifiedSample
#'
#'@inherit stratifiedSample return
#'
#'@details Balanced sampling is a technique of sampling from a population that
#'can be partitioned into 'strata' (or 'subpopulations'), where each element in
#'the population is part of one stratum. It is used to ensure that
#'subgroups of the population are equally represented in the taken sample.
#'Different approaches exists to take balanced samples from imbalanced populations.
#'
#'@inherit stratifiedSample author
#'
#'@references
#'He and Garcia, Learning from Imbalanced Data, IEEE Transactions on Knowledge
#'and Data Engineering (2009)
#'
#'@examples \dontrun{
#'#Set seed for reproducibility
#'set.seed(seed = 5381L)
#'
#'#Define strata
#'strata = c(rep("a", 3),rep("b", 6))
#'
#'#Check ratio
#'table(strata)/length(strata)
#'
#'#Balanced random sample without replacement
#'i = balancedSample(
#'   strata = strata,
#'   n = 6,
#'   replace = FALSE
#')
#'#Check ratio in the sample
#'table(strata[i])/length(strata[i])
#'
#'#Balanced random sample with replacement
#'i = balancedSample(
#'   strata = strata,
#'   n = 8,
#'   replace = TRUE
#')
#'#Check ratio in the sample
#'table(strata[i])/length(strata[i])
#'}
#'@keywords internal
balancedSample <- function(
    strata,
    n,
    replace = FALSE,
    prob = NULL
){
  #check input
  isValid = isValidStratifiedSampleInput(strata = strata, n = n, replace = replace)
  if(isFALSE(isTRUE(isValid))){
    stop(paste(isValid, collapse = "\n"))
  }

  N = length(strata)

  #cross tabulation
  xtable = table(strata)

  #Get the minimum number of observations per class (element != 0)
  min.nobs.class = min(xtable)

  #Get the strata names
  stratanames = names(xtable)

  #get n of levels
  n.strata = length(stratanames)

  #Create an empty vector
  out = NULL;

  #check
  if(isTRUE((n%%n.strata) != 0)){
    warning("Sample size is not multiple of number of strata. Balance will be approximated.\n")
    #number of elements we want per stratum
    n.per.stratum = ceiling(n / n.strata);
    #correct
    tmp = floor(n / n.strata)
    #rep
    n.per.stratum = c(n.per.stratum, rep(x = tmp, length = (n.strata-1)))
    #update names order
    stratanames = unique(c(names(which.max(xtable)), stratanames))
  } else {
    #number of elements we want per stratum
    n.per.stratum = n / n.strata;
    #rep
    n.per.stratum = rep(x = n.per.stratum, length = n.strata)
  }

  #Check it is possible
  if(isTRUE(!replace & any(n.per.stratum > min.nobs.class)) ){
    stop("The required sample size 'n' is too big to take a balanced sample ",
         "for the given 'strata'. Try to reduce 'n' or increase 'strata' by adding ",
         "elements from the minority subpopulation.")
  }

  #Loop over the strata to assign the samples evenly
  # i=2
  for(i in 1L:n.strata) {
    #Get the index of elements of current stratum
    el.index = which(strata==stratanames[i])
    #Randomly select the element we want per stratum
    index = base::sample(x = el.index, size = n.per.stratum[i], replace = replace, prob = prob[el.index])
    #update index vector
    out = c(out, index);
  }

  #force to have length n
  out = out[1:n]

  #permutation
  out = out[sample.int(n = n, size = n, replace = F)]

  #return
  return(out)
}

## Sample Without replacement --------------------------------------------------

#'Random Samples
#'
#'@description Takes a sample without replacement from the population.
#'See the **Details** section below for further information.
#'
#'@inheritParams randomSample
#'
#'@inherit randomSample return
#'
#'@inherit randomSample details
#'
#'@inherit randomSample author
#'
#'@examples
#'#Set seed for reproducibility
#'set.seed(seed = 5381L)
#'
#'#Simple random sample without replacement
#'sampleWithoutReplacement(N = 10, n = 5)
#'
#'@export
sampleWithoutReplacement <- function(
    N,
    n,
    prob = NULL
){
  #Select n random samples from the total observations
  out = randomSample(N = N, n = n, replace = FALSE, prob = prob);
  #return
  return(out)
}

#'Random Samples
#'
#'@description Takes a simple random sample without replacement from the population.
#'*Simple random sampling* (*SRS*) is the easiest form of sampling where each
#'element of the population has the same probability of being selected for the
#'sample.
#'See the **Details** section below for further information.
#'
#'@inheritParams randomSample
#'
#'@inherit randomSample return
#'
#'@inherit randomSample details
#'
#'@inherit randomSample author
#'
#'@examples
#'#Set seed for reproducibility
#'set.seed(seed = 5381L)
#'
#'#Simple random sample without replacement
#'simpleRandomSampleWithoutReplacement(N = 10, n = 5)
#'
#'@export
simpleRandomSampleWithoutReplacement <- function(
    N,
    n
){
  #Select n random samples from the total observations
  out = randomSample(N = N, n = n, replace = FALSE, prob = NULL);
  #return
  return(out)
}


#'Permutation Samples
#'
#'@description Takes a permutation sample.
#'See the **Details** section below for further information.
#'
#'@inheritParams randomSample
#'
#'@inherit randomSample return
#'
#'@inherit randomSample details
#'
#'@inherit randomSample author
#'
#'@examples
#'#Set seed for reproducibility
#'set.seed(seed = 5381L)
#'
#'#Random sequence of N elements from 1 to N
#'permutationSample(N = 10)
#'
#'@export
permutationSample <- function(
    N
){
  #Shuffle the input data
  out = randomSample(N = N, n = N, replace = FALSE);
  #return
  return(out)
}

#'Stratified Random Samples
#'
#'@description Takes a stratified sample without replacement from the population.
#'See the **Details** section below for further information.
#'
#'@inheritParams stratifiedSample
#'
#'@inherit stratifiedSample return
#'
#'@inherit stratifiedSample details
#'
#'@inherit stratifiedSample author
#'
#'@inherit stratifiedSample references
#'
#'@examples
#'#Set seed for reproducibility
#'set.seed(seed = 5381L)
#'
#'#Define strata
#'strata = c(rep("a", 3),rep("b", 6))
#'
#'#Check ratio
#'table(strata)/length(strata)
#'
#'#Stratified random sample
#'i = stratifiedSampleWithoutReplacement(
#'   strata = strata,
#'   n = 3
#')
#'#Check indices
#'i
#'#Check ratio in the sample
#'table(strata[i])/length(strata[i])
#'
#'@export
stratifiedSampleWithoutReplacement <- function(
    strata,
    n,
    prob = NULL
){

  #take sample
  out = stratifiedSample(strata = strata, n = n, replace = F, prob = prob)

  #return
  return(out)
}

#'Balanced Random Sample Without Replacement
#'
#'@description Takes a balanced sample without replacement from the population.
#'See the **Details** section below for further information.
#'
#'@inheritParams balancedSample
#'
#'@inherit balancedSample return
#'
#'@inherit balancedSample details
#'@details
#'When the number of elements per stratum (given by the sample size \code{n}
#'divided by the number of groups in \code{strata}) is less than the number of
#'elements in the minority group in \code{strata},
#'this function implements the so-called "random undersampling", in which
#'the proportion of the strata in the population is adjusted in the taken sample
#'by removing elements from the majority stratum, so that each group is balanced.
#'
#'When the number of elements per stratum is greater than the number of
#'elements in the minority group in \code{strata}, the function raises an error.
#'
#'@inherit balancedSample author
#'
#'@inherit balancedSample references
#'
#'@examples
#'#Set seed for reproducibility
#'set.seed(seed = 5381L)
#'
#'#Define strata
#'strata = c(rep("a", 3),rep("b", 6))
#'
#'#Check ratio
#'table(strata)/length(strata)
#'
#'#Balanced random sample
#'i = balancedSampleWithoutReplacement(
#'   strata = strata,
#'   n = 6
#')
#'#Check indices
#'i
#'#Check ratio in the sample
#'table(strata[i])/length(strata[i])
#'
#'@export
balancedSampleWithoutReplacement <- function(
    strata,
    n,
    prob = NULL
){
  #take sample
  out = balancedSample(strata = strata, n = n, replace = F, prob = prob)

  #return
  return(out)
}

## Sample With Replacement -----------------------------------------------------

#'Sample With Replacement
#'
#'@description Takes a sample with replacement from the population.
#'See the **Details** section below for further information.
#'
#'@inheritParams randomSample
#'
#'@inherit randomSample return
#'
#'@inherit randomSample details
#'
#'@inherit randomSample author
#'
#'@examples
#'#Set seed for reproducibility
#'set.seed(seed = 5381L)
#'
#'#Random sample with replacement
#'sampleWithReplacement(N = 10, n = 10)
#'
#'@export
sampleWithReplacement <- function(
    N,
    n,
    prob = NULL
){
  #Select n random samples from the total observations
  out = randomSample(N = N, n = n, replace = T, prob = prob);
  #return
  return(out)
}


#'Simple Random Sample With Replacement
#'
#'@description Takes a simple random sample with replacement from the population.
#'*Simple random sampling* (*SRS*) is the easiest form of sampling where each
#'element of the population has the same probability of being selected for the
#'sample.
#'See the **Details** section below for further information.
#'
#'@inheritParams randomSample
#'
#'@inherit randomSample return
#'
#'@inherit randomSample details
#'
#'@inherit randomSample author
#'
#'@examples
#'#Set seed for reproducibility
#'set.seed(seed = 5381L)
#'
#'#Random sample with replacement
#'simpleRandomSampleWithReplacement(N = 10, n = 10)
#'
#'@export
simpleRandomSampleWithReplacement <- function(
    N,
    n
){
  #Select n random samples from the total observations
  out = randomSample(N = N, n = n, replace = T);
  #return
  return(out)
}

#'Stratified Random Sample With Replacement
#'
#'@description Takes a stratified sample with replacement from the population.
#'See the **Details** section below for further information.
#'
#'@inheritParams stratifiedSample
#'
#'@inherit stratifiedSample return
#'
#'@inherit stratifiedSample details
#'
#'@inherit stratifiedSample author
#'
#'@inherit stratifiedSample references
#'
#'@examples
#'#Set seed for reproducibility
#'set.seed(seed = 5381L)
#'
#'#Define strata
#'strata = c(rep("a", 3),rep("b", 6))
#'
#'#Check ratio
#'table(strata)/length(strata)
#'
#'#Stratified random sample with replacement
#'i = stratifiedSampleWithReplacement(
#'   strata = strata,
#'   n = 3
#')
#'#Check indices
#'i
#'#Check ratio in the sample
#'table(strata[i])/length(strata[i])
#'
#'@export
stratifiedSampleWithReplacement <- function(
    strata,
    n,
    prob = NULL
){
  #take sample
  out = stratifiedSample(strata = strata, n = n, replace = T, prob = prob)

  #return
  return(out)
}

#'Balanced Random Sample With Replacement
#'
#'@description Takes a balanced sample with replacement from the population.
#'See the **Details** section below for further information.
#'
#'@inheritParams balancedSample
#'
#'@inherit balancedSample return
#'
#'@inherit balancedSample details
#'@details
#'This function works when the number of elements per stratum (given by the
#'sample size \code{n} divided by the number of groups in \code{strata}) is
#'less/greater than the number of elements in the minority group in
#'\code{strata}, by taking independent samples with replacement from each group.
#'
#'@inherit balancedSample author
#'
#'@inherit balancedSample references
#'
#'@examples
#'#Set seed for reproducibility
#'set.seed(seed = 5381L)
#'
#'#Define strata
#'strata = c(rep("a", 3),rep("b", 6))
#'
#'#Check ratio
#'table(strata)/length(strata)
#'
#'#Balanced random sample with replacement
#'i = balancedSampleWithReplacement(
#'   strata = strata,
#'   n = 8
#')
#'#Check indices
#'i
#'#Check ratio in the sample
#'table(strata[i])/length(strata[i])
#'
#'@export
balancedSampleWithReplacement <- function(
    strata,
    n,
    prob = NULL
){
  #take sample
  out = balancedSample(strata = strata, n = n, replace = T, prob = prob)

  #return
  return(out)
}


#'Ordinary Bootstrap Sample
#'
#'@description Takes a bootstrap sample from the population. A bootstrap sample
#'is a sample taken with replacement having the same size of the population.
#'See the **Details** section below for further information.
#'
#'@inheritParams randomSample
#'
#'@inherit randomSample return
#'
#'@inherit randomSample details
#'
#'@inherit randomSample author
#'
#'@examples
#'#Set seed for reproducibility
#'set.seed(seed = 5381L)
#'
#'#Random sample with replacement
#'bootstrapSample(N = 10)
#'
#'@export
bootstrapSample <- function(
    N,
    prob = NULL
){
  #take sample
  out = randomSample(N = N, n = N, replace = T, prob = prob);

  #return
  return(out)
}

# Leave-p-out Sampling ---------------------------------------------------------

#'Leave-p-out Sample Input Validator
#'
#'@description This function checks the input provided to the leave-p-out sampling
#'functions.
#'
#'@inheritParams leavePOutSample
#'
#'@return \code{TRUE} if the arguments are valid, or a string
#'containing the errors.
#'
#'@author Alessandro Barberis
#'
#'@examples \dontrun{
#'
#'#Valid input
#'isValidLpOSampleInput(
#'  N=10,
#'  p=3
#')
#'
#'#Invalid input
#'isValidLpOSampleInput(
#'  N=10,
#'  p=10
#')
#'}
#'@keywords internal
isValidLpOSampleInput <- function(
    N,
    p,
    prob
) {
  errors <- character()

  if (isTRUE(missing(N))){
    errors = "Missing input: 'N' must be provided."
    return(errors)
  } else {
    if (isFALSE(is.numeric(N) & N > 0 & length(N)==1)){
      return("'N' must be a positive integer of length 1.")
    }
  }

  if (isTRUE(missing(p))){
    errors = "Missing input: 'p' must be provided."
    return(errors)
  } else {
    if (isFALSE(is.numeric(p) & p > 0 & length(p)==1)){
      return("'p' must be a positive integer of length 1.")
    }
  }

  if (isTRUE(!missing(prob) && !is.null(prob))){
    if (isFALSE(is.numeric(prob) & all(prob >= 0))){
      msg <- paste("'prob' must contain positive numeric values. ", sep = "")
      errors <- c(errors, msg)
    }

    if (isFALSE(length(prob)==N)){
      msg <- paste("The vector of probability weights 'prob' is not of length 'N'. ", sep = "")
      errors <- c(errors, msg)
    }
  }

  if (isTRUE(p > N)) {
    msg <- paste0("Invalid holdout sample size: ",
    "'p' must be greater than zero and less than the population size 'N'. ")
    errors <- c(errors, msg)
  }

  errors = if (isTRUE(length(errors) == 0)) TRUE else errors

  return(errors)
}

#'Leave-P-Out Sampling
#'
#'@description Takes a random sample of size `N-p` from the population.
#'See the **Details** section below for further information.
#'
#'@inheritParams randomSample
#'@param p integer, the number of elements to holdout
#'
#'@return A vector of length \code{N-p} containing the index of the
#'computed random set of observations.
#'
#'@details A random sample of size `p` is taken from the population and used
#'as holdout data.
#'This function returns a sample of size `N-p` obtained by removing the holdout
#'sample from the population.
#'
#'@inherit randomSample author
#'
#'@examples
#'#Set seed for reproducibility
#'set.seed(seed = 5381L)
#'
#'#Take one sample leaving out p elements
#'leavePOutSample(N = 5, p = 2)
#'
#'#Equivalent to leave-one-out
#'leavePOutSample(N = 5, p = 1)
#'
#'@export
leavePOutSample <- function(
    N,
    p,
    prob = NULL){
  #check input
  isValid = isValidLpOSampleInput(N = N, p = p, prob = prob)
  if(isFALSE(isTRUE(isValid))){
    stop(paste(isValid, collapse = "\n"))
  }
  #get data to holdout
  holdout = sample.int(n = N, size = p, prob = prob, replace = F)
  #create output
  ##population
  p = seq(N)
  ##calculate sample
  out = setdiff(x = p, y = holdout)
  #return
  return(out)
}


#'Leave-One-Out Sampling
#'
#'@description Takes a random sample of size `N-1` from the population.
#'See the **Details** section below for further information.
#'
#'@inheritParams leavePOutSample
#'
#'@return A vector of length \code{N-1} containing the index of the
#'computed random set of observations.
#'
#'@details The leave-one-out is a particular case of the leave-p-out, where
#'`p = 1`.
#'A random sample of size `1` is taken from the population and used as holdout
#'data.
#'This function returns a sample of size `N-1` obtained by removing the holdout
#'sample from the population.
#'
#'@inherit leavePOutSample author
#'
#'@seealso
#'[`leavePOutSample`]
#'
#'@examples
#'#Set seed for reproducibility
#'set.seed(seed = 5381L)
#'
#'#Take one sample leaving out 1 element
#'leaveOneOutSample(N = 10)
#'
#'@export
leaveOneOutSample <- function(
    N){
  #compute
  out = leavePOutSample(N = N, p = 1L)
  #return
  return(out)
}

# K-Fold Sampling --------------------------------------------------------------

#'K-Folds Input Validator
#'
#'@description This function checks the input provided to the k-folds sampling
#'functions.
#'
#'@inheritParams randomKFolds
#'@inheritParams stratifiedKFolds
#'@inheritParams balancedKFolds
#'
#'@return \code{TRUE} if the arguments are valid, or a string
#'containing the errors.
#'
#'@author Alessandro Barberis
#'
#'@examples
#'\dontrun{
#'#Valid input
#'isValidCvInput(
#'  strata=c(1,1,1,2,2,2,2,2,2),
#'  k=3
#')
#'
#'#Invalid input
#'isValidCvInput(
#'  N=2,
#'  k=1
#')
#'
#'isValidCvInput(
#'  k=1
#')
#'}
#'@keywords internal
isValidCvInput <- function(
    N,
    k,
    strata,
    undersample,
    prob
) {
  errors <- character()

  if (isTRUE(missing(N))){
    if (isTRUE(missing(strata))){
      errors = "Missing input: 'N' or 'strata' must be provided."
      return(errors)
    } else {
      N = length(strata)
    }
  }

  if(isFALSE(is.numeric(N) & N > 0)){
    stop("'N' must be a positive integer.")
  }

  if(isFALSE(is.numeric(k) & k > 0)){
    stop("'k' must be a positive integer.")
  }

  if (isTRUE(k > N)) {
    msg <- paste("Number of folds 'k' greater than population size 'N'. ", sep = "")
    errors <- c(errors, msg)
  }

  if (isTRUE(k < 3)) {
    msg <- paste("Number of folds 'k' lower than 3 (i.e. the smallest value allowable of folds). ", sep = "")
    errors <- c(errors, msg)
  }

  if (isFALSE(missing(undersample))) {
    if (isFALSE(is.logical(undersample))){
      msg <- paste("'undersample' is not a logical value: it must be TRUE or FALSE.  ", sep = "")
      errors <- c(errors, msg)
    }
  }

  if (isTRUE(!missing(prob) && !is.null(prob))){
    if (isFALSE(is.numeric(prob) & all(prob >= 0))){
      msg <- paste("'prob' must contain positive numeric values. ", sep = "")
      errors <- c(errors, msg)
    }

    if (isFALSE(length(prob)==N)){
      msg <- paste("The vector of probability weights 'prob' is not the same",
                   "length of 'strata'. ", sep = "")
      errors <- c(errors, msg)
    }
  }

  errors = if (isTRUE(length(errors) == 0)) TRUE else errors

  return(errors)
}

#'Random K Folds
#'
#'@description Randomly assigns the population to k folds.
#'See the **Details** section below for further information.
#'
#'@param N population size
#'@param k number of folds
#'
#'@return A vector of length \code{N} containing the fold ids.
#'
#'@details Each element in the population is randomly assigned to one of the
#'k folds. Internally, it uses the \code{\link[base]{sample}} function.
#'
#'@author Alessandro Barberis
#'
#'@examples
#'#Set seed for reproducibility
#'set.seed(seed = 5381L)
#'
#'#Assign data to 3 folds
#'randomKFolds(N = 10, k = 3)
#'
#'@export
randomKFolds <- function(
    N,
    k
){

  #check input
  isValid = isValidCvInput(N = N, k = k)
  if(isFALSE(isTRUE(isValid))){
    stop(paste(isValid, collapse = "\n"))
  }

  #create folds
  out = base::sample(x = rep(x = seq(k), length = N), replace = F, size = N)

  #return
  return(out)
}


#'Stratified K Folds
#'
#'@description Assigns the population to k stratified folds.
#'See the **Details** section below for further information.
#'
#'@param strata vector of stratification variables. The population size is
#'\code{length(strata)}
#'@param k number of folds
#'
#'@return A vector of length \code{length(strata)} containing the fold ids.
#'
#'@details Each element in the population is assigned to one of the k folds so
#'that the percentage of each stratum in the population is preserved in each
#'fold.
#'
#'@author Alessandro Barberis
#'
#'@examples
#'#Set seed for reproducibility
#'set.seed(seed = 5381L)
#'
#'#Define strata
#'strata = c(1,1,1,2,2,2,2,2,2)
#'
#'#Check ratio
#'table(strata)/length(strata)
#'
#'#Assign data to 3 folds
#'i = stratifiedKFolds(
#'  strata = strata,
#'  k = 3
#')
#'#Check folds
#'i
#'#Check ratio in the folds
#'table(strata[i==1])/length(strata[i==1])
#'table(strata[i==2])/length(strata[i==2])
#'table(strata[i==3])/length(strata[i==3])
#'
#'@export
stratifiedKFolds <- function(
    strata = NULL,
    k
){

  #check input
  isValid = isValidCvInput(strata = strata, k = k)
  if(isFALSE(isTRUE(isValid))){
    stop(paste(isValid, collapse = "\n"))
  }

  #convert data
  k = as.integer(k)

  #get population size
  N = length(strata)

  #cross tabulation
  xtable = table(strata)

  #Get the minimum number of observations per class (element != 0)
  min.nobs.class = min(xtable)

  #Get the strata names
  stratanames = names(xtable)

  #get n of levels
  n.lvls = length(stratanames)

  #Create an empty vector
  out <- numeric(length = N);

  #Loop over the levels in the factor to assign the
  #samples evenly over the classes using the modulus operator
  # i=1
  for(i in 1L:n.lvls) {
    #Get the index of elements of current class
    el.index = which(strata==stratanames[i])
    #Get the number of elements for each class
    nclass = length(el.index);
    #Assign fold ids for the current class
    foldid.i <- sample.int(n = nclass, replace = F) %% k;
    #Assign the fold to the result vector
    out[el.index] <- foldid.i;
  }

  #Sum 1 to have the ids between 1 and nfolds
  out <- out + 1;

  #Return
  return(out)
}

#'Balanced K Folds
#'
#'@description Assigns the population to k balanced folds.
#'See the **Details** section below for further information.
#'
#'@param strata vector of stratification variables. The population size is
#'\code{length(strata)}
#'@param k number of folds
#'@inheritParams balancedSample
#'@param undersample logical, whether to remove elements from the population
#'in order to try to obtain balanced folds
#'
#'@return A vector of length \code{length(strata)} containing the fold ids.
#'
#'@details If the population is balanced, each element in the population is
#'assigned to one of the k folds so that the percentage of each stratum is
#'preserved in each fold.
#'If the population is unbalanced and \code{undersample = TRUE}, the so-called
#'"random undersampling" is adopted, i.e. the proportion of the strata in the
#'population is adjusted by removing elements from the majority groups, so that
#'each stratum is balanced.
#'An error is raised if the population is unbalanced and \code{undersample = FALSE}.
#'
#'@author Alessandro Barberis
#'
#'@examples
#'#Set seed for reproducibility
#'set.seed(seed = 5381L)
#'
#'#Define balanced strata
#'strata = c(rep(1,6),rep(2,6))
#'
#'#Check ratio
#'table(strata)/length(strata)
#'
#'#Assign data to 3 folds
#'i = balancedKFolds(
#'  strata = strata,
#'  k = 3
#')
#'#Check folds
#'i
#'#Check ratio in the folds
#'table(strata[i==1])/length(strata[i==1])
#'table(strata[i==2])/length(strata[i==2])
#'table(strata[i==3])/length(strata[i==3])
#'
#'#Define unbalanced strata
#'strata = c(rep(1,6),rep(2,12))
#'
#'#Check ratio
#'table(strata)/length(strata)
#'
#'#Assign data to 3 folds
#'i = balancedKFolds(
#'  strata = strata,
#'  k = 3,
#'  undersample = TRUE
#')
#'#Check folds
#'i
#'#Check ratio in the folds
#'table(strata[!is.na(i) & i==1])/length(strata[!is.na(i) & i==1])
#'table(strata[!is.na(i) & i==2])/length(strata[!is.na(i) & i==2])
#'table(strata[!is.na(i) & i==3])/length(strata[!is.na(i) & i==3])
#'
#'
#'#Raise an error
#'try(balancedKFolds(
#'  strata = strata,
#'  k = 3,
#'  undersample = FALSE
#'))
#'
#'@export
balancedKFolds <- function(
    strata = NULL,
    k,
    undersample = FALSE,
    prob = NULL
){

  #check input
  isValid = isValidCvInput(strata = strata, k = k, undersample = undersample, prob = prob)
  if(isFALSE(isTRUE(isValid))){
    stop(paste(isValid, collapse = "\n"))
  }

  #convert data
  k = as.integer(k)

  #get population size
  N = length(strata)

  #cross tabulation
  xtable = table(strata)

  #get the minimum number of observations per class (element != 0)
  min.nobs.class = min(xtable)

  #get the strata names
  stratanames = names(xtable)

  #get n of levels
  n.lvls = length(stratanames)

  #is the number of samples in each class equivalent to min.nobs.class?
  bool = xtable == min.nobs.class

  #check strata is balanced
  if(isFALSE(all(bool))){
    #data is not balanced
    if(isTRUE(undersample)){
      #try to balance the data by removing samples
      nclass = min.nobs.class
    } else {
      stop("Data is not balanced. Change 'undersample' to TRUE.")
    }
  } else {
    #data is balanced
    nclass = 0
  }

  #create an empty vector
  out <- rep(x = NA, times = N);

  #Loop over the levels in the factor to assign the
  #samples evenly over the classes using the modulus operator
  # i=1
  for(i in 1L:n.lvls) {
    #Get the index of elements of current class
    el.index = which(strata==stratanames[i])
    if(isFALSE(is.numeric(nclass) & nclass > 0)){
      #Get the number of elements for each class
      nclass = length(el.index)
    } else {
      #randomly select the elements we want to keep
      el.index = base::sample(x = el.index, size = nclass, replace = FALSE, prob = prob[el.index])
    }
    #Assign fold ids for the current class
    foldid.i <- sample.int(n = nclass, replace = F) %% k;
    #Assign the fold to the result vector
    out[el.index] <- foldid.i;
  }

  #Sum 1 to have the ids between 1 and nfolds
  out <- out + 1;

  #Return
  return(out)

}

#'K-1 folds
#'
#'@description Takes a population assigned to k folds and returns a random sample
#'made of elements from k-1 folds.
#'
#'@param foldid vector of k fold ids
#'@param i (optional) integer, fold to be use as holdout data
#'
#'@return A vector containing the indices of the sampled data.
#'
#'@details  The returned sample is a vector of indices generated by removing the
#'holdout fold and merging the remaining k - 1 folds together.
#'If `i` is missing, one fold is randomly selected to be the holdout data.
#'
#'@author Alessandro Barberis
#'
#'@examples \dontrun{
#'#set seed for reproducibility
#'set.seed(seed = 5381L)
#'
#'#Assign data to 3 folds
#'km1Folds(
#'  foldid = randomKFolds(N = 10, k = 3)
#')
#'}
#'
#'@keywords internal
km1Folds <- function(
    foldid,
    i = NULL
){
  #sample
  ##get max fold id
  nfolds = max(foldid, na.rm = T)

  ##check input
  if(isTRUE(missing(i) || is.null(i))){
    ##holdout fold
    i = sample.int(n = nfolds, size = 1)
  } else {
    if(isFALSE(is.numeric(i) && i > 0 && i <= nfolds)){
      stop("If provided, 'i' must be an integer between [1, k].")
    }
  }

  ##keep only the training sets
  keep = !is.na(foldid) & foldid != i

  ##output
  out = which(keep)

  #return
  return(out)
}



#'K-1 Random Folds
#'
#'@description Randomly assigns the population to k folds and returns a random sample
#'made of elements from k-1 folds.
#'See the **Details** section below for further information.
#'
#'@inheritParams randomKFolds
#'@inheritParams km1Folds
#'
#'@inherit repeatedKm1Folds return
#'
#'@details Each element in the population is randomly assigned to one of the k
#'folds by using the [`randomKFolds`] function.
#'If provided, `i` indicates the i-th fold to be considered as holdout data.
#'If `i` is missing, one fold is randomly selected to be the holdout data.
#'A random sample is then generated by removing the i-th fold and merging the
#'remaining k - 1 folds together.
#'
#'@inherit km1Folds author
#'
#'@seealso
#'[`randomKFolds`]
#'
#'@examples
#'#Set seed for reproducibility
#'set.seed(seed = 5381L)
#'
#'#Assign data to 3 folds
#'randomKm1Folds(N = 10, k = 3)
#'
#'@export
randomKm1Folds <- function(
    k,
    N,
    i = NULL){
  #get folds
  foldid = randomKFolds(N = N, k = k)
  #get sample made of k-1 folds
  out = km1Folds(foldid = foldid, i = i)
  #return
  return(out)
}

#'K-1 Stratified Folds
#'
#'@description Assigns the population to k stratified folds and returns a random
#'sample made of elements from k-1 folds.
#'See the **Details** section below for further information.
#'
#'@inheritParams stratifiedKFolds
#'@inheritParams km1Folds
#'
#'@inherit km1Folds return
#'
#'@details Each element in the population is randomly assigned to one of the k
#'folds - so that the percentage of each stratum in the population is preserved
#'in each fold - by using the [`stratifiedKFolds`] function.
#'If provided, `i` indicates the i-th fold to be considered as holdout data.
#'If `i` is missing, one fold is randomly selected to be the holdout data.
#'A random sample is then generated by removing the i-th fold and merging the
#'remaining k - 1 folds together.
#'
#'@inherit km1Folds author
#'
#'@seealso
#'[`stratifiedKFolds`]
#'
#'@examples
#'#Set seed for reproducibility
#'set.seed(seed = 5381L)
#'
#'#Define strata
#'strata = c(1,1,1,2,2,2,2,2,2)
#'
#'#Check ratio
#'table(strata)/length(strata)
#'
#'#Assign data to 3 folds
#'i = repeatedStratifiedKm1Folds(
#'  strata = strata,
#'  k = 3
#')
#'#Check indices
#'i
#'#Check ratio in the samples made of k-1 folds
#'table(strata[i[[1]]])/length(strata[i[[1]]])
#'table(strata[i[[2]]])/length(strata[i[[2]]])
#'table(strata[i[[3]]])/length(strata[i[[3]]])
#'
#'@export
stratifiedKm1Folds <- function(
    k,
    strata,
    i = NULL
  ){
  #get folds
  foldid = stratifiedKFolds(strata = strata, k = k)
  #get sample made of k-1 folds
  out = km1Folds(foldid = foldid, i = i)
  #return
  return(out)
}

#'K-1 Balanced Folds
#'
#'@description Assigns the population to k balanced folds and returns a random
#'sample made of elements from k-1 folds.
#'See the **Details** section below for further information.
#'
#'@inheritParams balancedKFolds
#'@inheritParams km1Folds
#'
#'@inherit km1Folds return
#'
#'@details Each element in the population is randomly assigned to one of the k
#'folds so that the percentage of each stratum in the population is balanced
#'in each fold (see [`balancedKFolds`] function for further details).
#'If provided, `i` indicates the i-th fold to be considered as holdout data.
#'If `i` is missing, one fold is randomly selected to be the holdout data.
#'A random sample is then generated by removing the i-th fold and merging the
#'remaining k - 1 folds together.
#'
#'@inherit km1Folds author
#'
#'@seealso
#'[`balancedKFolds`]
#'
#'@examples
#'#Set seed for reproducibility
#'set.seed(seed = 5381L)
#'
#'#Define balanced strata
#'strata = c(rep(1,6),rep(2,6))
#'
#'#Check ratio
#'table(strata)/length(strata)
#'
#'#Assign data to 3 folds
#'i = balancedKm1Folds(
#'  strata = strata,
#'  k = 3
#')
#'#Check indices
#'i
#'#Check ratio in the samples made of k-1 folds
#'table(strata[i])/length(strata[i])
#'
#'#Define unbalanced strata
#'strata = c(rep(1,6),rep(2,12))
#'
#'#Check ratio
#'table(strata)/length(strata)
#'
#'#Assign data to 3 folds
#'i = balancedKm1Folds(
#'  strata = strata,
#'  k = 3,
#'  undersample = TRUE
#')
#'#Check folds
#'i
#'#Check ratio in the samples made of k-1 folds
#'table(strata[i])/length(strata[i])
#'
#'@export
balancedKm1Folds <- function(
    k,
    strata,
    undersample = FALSE,
    prob = NULL,
    i = NULL){
  #get folds
  foldid = balancedKFolds(
    strata = strata,
    k = k,
    undersample = undersample,
    prob = prob)
  #get sample made of k-1 folds
  out = km1Folds(foldid = foldid, i = i)
  #return
  return(out)
}


# Get Sampling Functions -------------------------------------------------------

#'Get Sampling Function
#'
#'@description This function is a dispatcher for the sampling function selected
#'in input.
#'
#'@param id character string, one of the supported sampling techniques
#'@return A sampling function:
#'
#'\describe{
#' \item{\code{"rswor"               }}{\code{\link{sampleWithoutReplacement}}}
#' \item{\code{"srswor"              }}{\code{\link{simpleRandomSampleWithoutReplacement}}}
#' \item{\code{"stratified_rswor"    }}{\code{\link{stratifiedSampleWithoutReplacement}}}
#' \item{\code{"balanced_rswor"      }}{\code{\link{balancedSampleWithoutReplacement}}}
#' \item{\code{"permutation"         }}{\code{\link{permutationSample}}}
#' \item{\code{"kfolds"              }}{\code{\link{randomKm1Folds}}}
#' \item{\code{"stratified_kfolds"   }}{\code{\link{stratifiedKm1Folds}}}
#' \item{\code{"balanced_kfolds"     }}{\code{\link{balancedKm1Folds}}}
#' \item{\code{"leave_p_out"         }}{\code{\link{leavePOutSample}}}
#' \item{\code{"leave_one_out"       }}{\code{\link{leaveOneOutSample}}}
#' \item{\code{"rswr"                }}{\code{\link{sampleWithReplacement}}}
#' \item{\code{"srswr"               }}{\code{\link{simpleRandomSampleWithReplacement}}}
#' \item{\code{"stratified_rswr"     }}{\code{\link{stratifiedSampleWithReplacement}}}
#' \item{\code{"balanced_rswr"       }}{\code{\link{balancedSampleWithReplacement}}}
#' \item{\code{"bootstrap"           }}{\code{\link{bootstrapSample}}}
#'}
#'
#'@examples
#'f = getSamplingFunction(id = 'rswor')
#'
#'@export
getSamplingFunction <- function(
    id = c(
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
    )
  ){

  #match arg
  id = match.arg(id)

  #get function
  f = switch(
    id,
    "rswor"                = sampleWithoutReplacement,
    "srswor"               = simpleRandomSampleWithoutReplacement,
    "stratified_rswor"     = stratifiedSampleWithoutReplacement,
    "balanced_rswor"       = balancedSampleWithoutReplacement,
    "permutation"          = permutationSample,
    "kfolds"               = randomKm1Folds,
    "stratified_kfolds"    = stratifiedKm1Folds,
    "balanced_kfolds"      = balancedKm1Folds,
    "leave_p_out"          = leavePOutSample,
    "leave_one_out"        = leaveOneOutSample,
    "rswr"                 = sampleWithReplacement,
    "srswr"                = simpleRandomSampleWithReplacement,
    "stratified_rswr"      = stratifiedSampleWithReplacement,
    "balanced_rswr"        = balancedSampleWithReplacement,
    "bootstrap"            = bootstrapSample
  )

  #return
  return(f)
}
