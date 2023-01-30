# Default Values ---------------------------------------------------------------

#'Package Name
#'
#'@description This function returns the name of the package.
#'
#'@return A string containing the name of the package.
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
getThisPackageName <- function(){
  name = "resampling"
  return(name)
}

# List Available Methods -------------------------------------------------------

#'Available Random Sampling With Replacement Method Ids
#'
#'@description This function returns the ids of the currently available methods
#'for random sampling with replacement.
#'
#'@return A vector containing the sampling method identifiers.
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
listAvailableSamplingWithReplacementMethodIds <- function(){
  #method ids
  out = c(
    "rswr",
    "srswr",
    "stratified_rswr",
    "balanced_rswr",
    "bootstrap"
  )

  return(out)
}

#'Available Random Sampling Without Replacement Method Ids
#'
#'@description This function returns the ids of the currently available methods
#'for random sampling without replacement.
#'
#'@return A vector containing the sampling method identifiers.
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
listAvailableSamplingWithoutReplacementMethodIds <- function(){
  #method ids
  out = c(
    "rswor",
    "srswor",
    "stratified_rswor",
    "balanced_rswor",
    "permutation",
    "kfolds",
    "stratified_kfolds",
    "balanced_kfolds",
    "leave_p_out",
    "leave_one_out"
  )

  return(out)
}

#'Available Random Sampling Method IDs
#'
#'@description This function returns the currently available
#'random sampling methods.
#'
#'@param x character, whether to return all sampling methods (`all`) or only the
#'methods using sampling with (`rswr`) or without (`rswor`) replacement
#'
#'@return A vector containing the sampling method identifiers.
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
listAvailableSamplingMethodIds <- function(x = c('all', 'rswor', 'rswr')){
  x = match.arg(x)
  #method ids
  out = switch(
    x,
    'all' = c(
      listAvailableSamplingWithoutReplacementMethodIds(),
      listAvailableSamplingWithReplacementMethodIds()
    ),
    'rswor' = listAvailableSamplingWithoutReplacementMethodIds(),
    'rswr'  = listAvailableSamplingWithReplacementMethodIds()
  )

  return(out)
}

#'Sampling Method Name
#'
#'@description This function returns the name of the sampling method in input.
#'
#'@return A character string, the name of the random sampling method.
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
getSamplingMethodName <- function(
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
  #match
  id = match.arg(id)
  #switch
  out = switch(
    id,
    "rswor"                = "random sampling without replacement",
    "srswor"               = "simple random sampling without replacement",
    "stratified_rswor"     = "stratified random sampling without replacement",
    "balanced_rswor"       = "balanced random sampling without replacement",
    "permutation"          = "permutation sampling",
    "kfolds"               = "random k-fold sampling",
    "stratified_kfolds"    = "stratified k-fold sampling",
    "balanced_kfolds"      = "balanced k-fold sampling",
    "leave_p_out"          = "leave-p-out sampling",
    "leave_one_out"        = "leave-one-out sampling",
    "rswr"                 = "random sampling with replacement",
    "srswr"                = "simple random sampling with replacement",
    "stratified_rswr"      = "stratified random sampling with replacement",
    "balanced_rswr"        = "balanced random sampling with replacement",
    "bootstrap"            = "ordinary bootstrap sampling"
  )
  #return
  return(out)
}

#'Available Random Sampling Methods
#'
#'@description This function returns the currently available
#'random sampling methods.
#'@inheritParams listAvailableSamplingMethodIds
#'@return A data frame with two columns:
#'
#'\describe{
#'\item{id}{the id of the random sampling method, to be used in the function calls}
#'\item{name}{the name of the random sampling method}
#'}
#'
#'@author Alessandro Barberis
#'
#'@examples
#'
#'listAvailableSamplingMethods()
#'
#'@export
listAvailableSamplingMethods <- function(x = c('all', 'rswor', 'rswr')){
  x = match.arg(x)
  #method ids
  ids = listAvailableSamplingMethodIds(x=x)
  #method names
  nm = sapply(X = ids, FUN = getSamplingMethodName)
  #output
  out = data.frame(
    id = ids,
    name = nm,
    row.names = NULL,
    stringsAsFactors = F
  )

  return(out)
}


#'Sampling Function Name
#'
#'@description This function returns the name of the sampling function in input.
#'
#'@return A character string, the name of the random sampling function.
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
getSamplingFunctionName <- function(
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
  #match
  id = match.arg(id)
  #switch
  out = switch(
    id,
    "rswor"                = "sampleWithoutReplacement",
    "srswor"               = "simpleRandomSampleWithoutReplacement",
    "stratified_rswor"     = "stratifiedSampleWithoutReplacement",
    "balanced_rswor"       = "balancedSampleWithoutReplacement",
    "permutation"          = "permutationSample",
    "kfolds"               = "randomKm1Folds",
    "stratified_kfolds"    = "stratifiedKm1Folds",
    "balanced_kfolds"      = "balancedKm1Folds",
    "leave_p_out"          = "leavePOutSample",
    "leave_one_out"        = "leaveOneOutSample",
    "rswr"                 = "sampleWithReplacement",
    "srswr"                = "simpleRandomSampleWithReplacement",
    "stratified_rswr"      = "stratifiedSampleWithReplacement",
    "balanced_rswr"        = "balancedSampleWithReplacement",
    "bootstrap"            = "bootstrapSample"
  )
  #return
  return(out)
}


#'Available Random Sampling Functions
#'
#'@description This function returns the currently available
#'random sampling functions.
#'@inheritParams listAvailableSamplingMethodIds
#'@return A data frame with two columns:
#'
#'\describe{
#'\item{id}{the id of the random sampling method, to be used in the function calls}
#'\item{name}{the name of the random sampling function}
#'}
#'
#'@author Alessandro Barberis
#'
#'@examples
#'
#'listSamplingFunctionNames()
#'
#'@export
listSamplingFunctionNames <- function(x = c('all', 'rswor', 'rswr')){
  x = match.arg(x)
  #method ids
  ids = listAvailableSamplingMethodIds(x=x)
  #method names
  nm = sapply(X = ids, FUN = getSamplingFunctionName)
  #output
  out = data.frame(
    id = ids,
    name = nm,
    row.names = NULL,
    stringsAsFactors = F
  )

  return(out)
}


#'Resampling Function Name
#'
#'@description This function returns the name of the resampling function in input.
#'
#'@return A character string, the name of the resampling function.
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
getResamplingFunctionName <- function(
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
  #match
  id = match.arg(id)
  #switch
  out = switch(
    id,
    "rswor"                = "repeatedSampleWithoutReplacement",
    "srswor"               = "repeatedSimpleRandomSampleWithoutReplacement",
    "stratified_rswor"     = "repeatedStratifiedSampleWithoutReplacement",
    "balanced_rswor"       = "repeatedBalancedSampleWithoutReplacement",
    "permutation"          = "repeatedPermutationSample",
    "kfolds"               = "repeatedRandomKm1Folds",
    "stratified_kfolds"    = "repeatedStratifiedKm1Folds",
    "balanced_kfolds"      = "repeatedBalancedKm1Folds",
    "leave_p_out"          = "repeatedLeavePOutSample",
    "leave_one_out"        = "repeatedLeaveOneOutSample",
    "rswr"                 = "repeatedSampleWithReplacement",
    "srswr"                = "repeatedSimpleRandomSampleWithReplacement",
    "stratified_rswr"      = "repeatedStratifiedSampleWithReplacement",
    "balanced_rswr"        = "repeatedBalancedSampleWithReplacement",
    "bootstrap"            = "repeatedBootstrapSample"
  )
  #return
  return(out)
}


#'Available Random Resampling Functions
#'
#'@description This function returns the currently available
#'random resampling functions.
#'
#'@inheritParams listAvailableSamplingMethodIds
#'
#'@return A data frame with two columns:
#'
#'\describe{
#'\item{id}{the id of the random sampling method, to be used in the function calls}
#'\item{name}{the name of the random resampling function}
#'}
#'
#'@author Alessandro Barberis
#'
#'@examples
#'
#'listResamplingFunctionNames()
#'
#'@export
listResamplingFunctionNames <- function(x = c('all', 'rswor', 'rswr')){
  x = match.arg(x)
  #method ids
  ids = listAvailableSamplingMethodIds(x=x)
  #method names
  nm = sapply(X = ids, FUN = getResamplingFunctionName)
  #output
  out = data.frame(
    id = ids,
    name = nm,
    row.names = NULL,
    stringsAsFactors = F
  )

  return(out)
}


