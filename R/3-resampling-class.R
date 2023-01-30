#'@include 2-resampling-functions.R
NULL

# Resampling Class -------------------------------------------------------------

#'Constructor of the `resampling` Class
#'
#'@description This is the low-level constructor of the \code{resampling} class
#'representing a series of samples repeatedly taken from a population.
#'
#'@param method character, the id of the used sampling method
#'@param N integer, the population size
#'@param removed integer, the indices of elements removed prior to sampling
#'@param samples list, each element is an integer vector containing the samples
#'
#'@return An object of class `resampling`.
#'
#'@details An object of class `resampling` is a named list with 4 elements:
#'\describe{
#' \item{`method`}{the id of the used sampling method}
#' \item{`N`}{the size of the population from which the samples were taken.
#' Elements in the population have index from 1 to N}
#' \item{`removed`}{(optional) vector of indices of elements removed from the
#' population before taking the samples}
#' \item{`samples`}{list of samples repeatedly taken from the population. Each
#' element of the list is an integer vector containing the indices of the
#' elements sampled from the population}
#'}
#'
#'Functions to facilitate access to the data stored in a `resampling` object are
#'available:
#'
#'* `?getSamplingMethodId`: returns the sampling method id
#'* `?getPopulationSize`: returns the population size
#'* `?getRemovedElements`: returns the indices of elements removed from the population
#'* `?getNumberOfSamples`: returns the number of taken samples
#'* `?getSamples`: returns the list of samples
#'* `?getSampleSize`: returns a vector of integer values, the size of each sample
#'* `?getHoldOutSample`: returns a list where each item is an integer vector
#'containing the indices of the elements not sampled from the population
#'* `?getHoldOutSampleSize`:returns a vector of integer values, the size of each
#'hold-out sample
#'
#'Other useful functions include:
#'
#'* \code{\link[=print.resampling]{print}}: print a summary of the `resampling` object
#'* \code{\link[=plot.resampling]{plot}}: plot the samples taken from the population as an heatmap
#'
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
newResampling <- function(
    method  = character(),
    N       = integer(),
    removed = integer(),
    samples = list(integer())
  ) {

  stopifnot(is.character(method))
  stopifnot(is.integer(N))
  stopifnot(is.integer(removed))
  stopifnot(is.list(samples))
  stopifnot(is.integer(unlist(x = samples, recursive = T)))

  structure(
    .Data = list(
      method  = method,
      N       = N,
      removed = removed,
      samples = samples
    ),
    class = "resampling"
  )
}

#'Validator of `resampling` Objects
#'
#'@description This is the validator of the `resampling` objects.
#'
#'@param x a \code{\link{resampling}} object
#'
#'@inherit newResampling return
#'
#'@inherit newResampling author
#'
#'@examples \dontrun{
#'#No error is raised
#'validateResampling(newResampling())
#'
#'#An error is raised
#'validateResampling(
#' newResampling(
#'  method = 'random',
#'  N = 3,
#'  samples = list(c(1, 4))
#' )
#')
#'}
#'@keywords internal
validateResampling <- function(x){
  #Check population size
  if(isTRUE(length(x$N)==1)){
    if(isFALSE(x$N > 0)){
      stop("'N' must be a positive integer value.", call. = F)
    }
  } else if(isTRUE(length(x$N)>1)){
    stop("'N' must be of length 1.", call. = F)
  }
  #Check indices of removed elements
  if(isTRUE(length(x$removed) > 0)){
    if(isFALSE(all(!is.na(x$removed) & x$removed > 0 & x$removed <= x$N))){
      stop("All 'removed' values must be non-missing, greater than zero, and ",
      "less than or equal to 'N' values.", call. = F)
    }
  }
  #Check indices of sampled elements
  if(isTRUE(length(x$samples) > 0)){
    indices = unlist(x$samples)
    if(isFALSE(all(!is.na(indices) & indices>0 & indices<=x$N))){
      stop("'samples' must be a list of vectors containing non-missing, positive",
      "less than or equal to 'N' values.",
           call. = F)
    }
  }
  #return
  return(x)
}

#'Constructor of the `resampling` class
#'
#'@description This is the constructor of the `resampling` class.
#@description Functions to construct and check `resampling` objects.
#'
#'@inheritParams newResampling
#'
#'@inherit newResampling return
#'
#'@inherit newResampling details
#'
#'@inherit newResampling author
#'
#'@examples
#'#default
#'resampling()
#'
#'#resampling object
#'resampling(
#'  method = 'rswor',
#'  N = 10,
#'  samples = list(c(1,5,7))
#')
#'
#'@export
#@keywords internal
resampling <- function(
    method  = c(
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
    N       = integer(),
    removed = integer(),
    samples = list(integer())
){
  #coerce input
  N = as.integer(N)
  removed = as.integer(removed)
  samples = lapply(X = samples, FUN = as.integer)

  #check input
  method = match.arg(method)

  #create object
  o = newResampling(
    method  = method,
    N       = N,
    removed = removed,
    samples = samples
  )

  #validate
  o = validateResampling(o)

  #return
  return(o)
}

# Getters ----------------------------------------------------------------------

#'Get the sampling method id
#'
#'@description Get the sampling method id from a [`resampling`] object.
#'
#'@param x an object of class [`resampling`]
#'
#'@return A character string, the sampling method id.
#'
#'@author Alessandro Barberis
#'
#'@export
getSamplingMethodId <- function(x){
  #return
  return(x$method)
}

#'Get the population size
#'
#'@description Get the population size from a [`resampling`] object.
#'
#'@param x an object of class [`resampling`]
#'
#'@return An integer value, the population size.
#'
#'@author Alessandro Barberis
#'
#'@export
getPopulationSize <- function(x){
  return(x$N)
}

#'Get the removed elements
#'
#'@description Get the elements removed from the population before sampling
#'from a [`resampling`] object.
#'
#'@param x an object of class [`resampling`]
#'
#'@return A vector of integer values, the indices of elements removed from the
#'population.
#'
#'@author Alessandro Barberis
#'
#'@export
getRemovedElements <- function(x){
  return(x$removed)
}

#'Get the number of taken samples
#'
#'@description Get the number of taken samples from a [`resampling`] object.
#'
#'@param x an object of class [`resampling`]
#'
#'@return The number of taken samples.
#'
#'@author Alessandro Barberis
#'
#'@export
getNumberOfSamples <- function(x){
  return(as.integer(length(x$samples)))
}

#'Get the sampled data
#'
#'@description Get the list of samples from a [`resampling`] object.
#'
#'@param x an object of class [`resampling`]
#'
#'@return A list of samples repeatedly taken from the population. Each
#'item of the list is an integer vector containing the indices of the
#'elements sampled from the population.
#'
#'@author Alessandro Barberis
#'
#'@export
getSamples <- function(x){
  return(x$samples)
}

#'Get the sample size
#'
#'@description Get the sample size from a [`resampling`] object.
#'
#'@param x an object of class [`resampling`]
#'
#'@return A vector of integer values, the size of each sample in \code{x}.
#'
#'@author Alessandro Barberis
#'
#'@export
getSampleSize <- function(x){
  #get samples
  x = getSamples(x)
  #calculate size
  x = sapply(X = x, length)
  #return
  return(x)
}

#'Get the left-out data
#'
#'@description Get the elements of the population not selected in the samples.
#'
#'@param x an object of class [`resampling`]
#'
#'@return A list where each item is an integer vector containing the indices of
#'the elements not sampled from the population.
#'
#'@author Alessandro Barberis
#'
#'@export
getHoldOutSamples <- function(x){
  #population
  i = seq(x$N)
  #update
  i = setdiff(x = i, y = x$removed)
  #remove sampled data from population
  out = lapply(X = x$samples, FUN = setdiff, x = i)
  #return
  return(out)
}

#'Get the hold-out sample size
#'
#'@description Get the hold-out sample size from a [`resampling`] object.
#'
#'@param x an object of class [`resampling`]
#'
#'@return A vector of integer values, the size of each hold-out sample in \code{x}.
#'
#'@author Alessandro Barberis
#'
#'@export
getHoldOutSampleSize <- function(x){
  #get samples
  x = getHoldOutSamples(x)
  #calculate size
  x = sapply(X = x, length)
  #return
  return(x)
}

# Methods ----------------------------------------------------------------------

#'Check a `resampling` object
#'
#@describeIn resampling returns `TRUE` if its argument is a valid [`resampling`]
#object.
#'@description Function to check if an object is a [`resampling`] object.
#'
#'@param x an object of class [`resampling`]
#'
#'@return Returns `TRUE` if its argument is a valid [`resampling`] object.
#'
#'@author Alessandro Barberis
#'
#'@examples
#'#valid object
#'is.resampling(x = resampling())
#'
#'#invalid object
#'is.resampling(x = 3)
#'
#'@export
is.resampling <- function(x){
  bool = TRUE
  #check structure
  if(isFALSE(is.list(x) & identical(class(x), "resampling") & length(x)==4 &
             all(names(x) %in% names(newResampling())) )){
    return(FALSE)
  } else {
    bool = tryCatch(
      expr = {
        x = validateResampling(x)
        return(TRUE)
      },
      error = function(e){return(FALSE)}
    )
  }
  return(bool)
}

#'Coerce to a data frame
#'
#'@description Function to coerce a [`resampling`] object to a data frame.
#'
#'@param x an object of class [`resampling`]
#'@param ... additional arguments to [`data.frame`]
#'@inheritParams base::data.frame
#'
#'@return A data frame.
#'
#'@author Alessandro Barberis
#'
#'@examples
#'#resampling object
#'x = resampling(
#'  method = 'rswor',
#'  N = 10,
#'  samples = list(c(1,5,7), c(6,2,9))
#')
#'
#'#print
#'as.data.frame(x)
#'
#'@export
as.data.frame.resampling <- function(x, ..., stringsAsFactors = FALSE){
  #number of taken samples
  numOfSamples = getNumberOfSamples(x)
  #sample number
  sampleNumber = seq(numOfSamples)
  #samples
  ##get samples
  samples = getSamples(x)
  ##create a string per item
  samples = sapply(X = samples, FUN = paste, sep = " ", collapse = ", ")
  #sample size
  sampleSize = getSampleSize(x)
  #hold-out sample size
  holdoutSize = getHoldOutSampleSize(x)
  #output object
  out = data.frame(
    sampleNumber = sampleNumber,
    sample       = samples,
    sampleSize   = sampleSize,
    holdoutSize  = holdoutSize,
    ...,
    stringsAsFactors = stringsAsFactors
  )

  #return
  return(out)
}

#'Print a `resampling` object
#'
#'@description Print a summary of the [`resampling`] object.
#'
#'@param x an object of class [`resampling`]
#'@param nrows integer, the number of samples to show. If more than one value is
#'provided, it indicates the indices of samples to show
#'@param nchars integer, the maximum number of characters to show in the summary
#'table
#'@param ... additional print arguments
#'
#'@return Silently return \code{x}.
#'
#'@examples
#'#resampling object
#'x = resampling(
#'  method = 'rswor',
#'  N = 10,
#'  samples = list(c(1,5,7), c(6,2,9))
#')
#'
#'#print
#'print(x)
#'
#'@export print.resampling
#'@export
print.resampling <- function(x, nrows = 5L, nchars = 10L, ...){
  #check input
  stopifnot(is.numeric(nrows))
  stopifnot(is.numeric(nchars))

  #coerce
  nrows = as.integer(nrows)
  nchrs = as.integer(nchars)

  #get data
  ##number of taken samples
  numOfSamples = getNumberOfSamples(x)
  ##population size
  populationSize = getPopulationSize(x)
  ##sampling method
  samplingMethodId = getSamplingMethodId(x)

  cat("\n")
  #summary
  cat(
    strwrap(
      x = paste0(
        numOfSamples, " samples",
        " taken from a population of ", populationSize, " elements",
        " by using ", getSamplingMethodName(samplingMethodId),
        ".")
    ),
    sep = "\n"
  )

  cat("\n")

  #samples
  df = as.data.frame.resampling(x)

  #update rows to show
  nrows = sapply(X = nrows, FUN = min, numOfSamples)
  nrows = unique(nrows)
  if(isTRUE(length(nrows)==1L)){nrows = seq(nrows)}
  df = df[nrows, , drop=F]

  #update string length
  ##calculate length
  stringLength = sapply(X = df$sample, FUN = nchar)
  ##check if length is greater than nchars
  isGtNchars = stringLength > nchars
  ##update
  df$sample[isGtNchars] = paste0(
    substr(x = df$sample[isGtNchars], start = 1L, stop = nchars),
    ", ...")
  ##print
  print(df)
  if(isTRUE(nrow(df)<numOfSamples)){cat("...", sep = "\n")}

  cat("\n")
  #return
  invisible(x)
}

#'Combine samples from `resampling` objects
#'
#'@description Combine the list of samples from [`resampling`] objects obtained
#'using the same sampling method and population size.
#'
#'@param e1,e2 object of class [`resampling`]
#'
#'@return An object of class [`resampling`] where the `samples` slot is the
#'combination of the samples of `e1` and `e2`.
#'
#'@author Alessandro Barberis
#'
#@export
#'@keywords internal
`+.resampling` <- function(e1, e2){
  #check n of arguments
  if (isTRUE(nargs() == 1L))
    return(e1)
  #check arguments
  if(isTRUE(inherits(x = e1, what = "resampling") &&
            inherits(x = e2, what = "resampling"))){
    tmp1 = e1
    tmp2 = e2
    tmp1$samples = list()
    tmp2$samples = list()

    if(isTRUE(identical(x = tmp1, y = tmp2))){
      out = resampling(
        method = getSamplingMethodId(e1),
        N = getPopulationSize(e1),
        removed = getRemovedElements(e1),
        samples = c(getSamples(e1), getSamples(e2))
      )
    } else {
      stop("Binary '+' is not defined for 'resampling' objects obtained from different sampling methods or population.")
    }
    rm(tmp1,tmp2)
  } else {
    stop("Binary '+' is defined only for 'resampling' objects.")
  }
  return(out)
}


#'Map Strata to Colors
#'
#'@description This function maps the strata variables to colors.
#'
#'@param strata vector of stratification variables
#'@param palette character, a RColorBrewer palette name. If `auto`, an automatic
#'selection based on the number of stratification variables is made
#'
#'@return A vector of colors.
#'
#'@author Alessandro Barberis
#'
#'
#'@keywords internal
mapStrataToColorsWithColorBrewer <- function(
    strata,
    palette = c(
      "auto",
      #sequential palettes
      "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd",
      "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu",
      "YlOrBr", "YlOrRd",
      #qualitative palettes
      "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3",
      #divergin palettes
      "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn",
      "Spectral"
    )
){
  #match
  rcbpalette = match.arg(palette)

  #number of levels
  n = length(levels(factor(strata)))

  #check
  if(isTRUE(identical(rcbpalette, "auto"))){
    if(isTRUE(n < 9)){
      rcbpalette = "Dark2"
    }else if(isTRUE(n < 10)){
      rcbpalette = "Set1"
    }else if(isTRUE(n < 13)){
      rcbpalette = "Set3"
    }else{
      stop("Maximum number of colors is 12.")
    }
  }

  #colors
  col = RColorBrewer::brewer.pal(
    n = if(isTRUE(n>2)){n}else{3},
    name = rcbpalette
  )
  col = col[seq(n)]
  return(col)
}

#'Map Strata to Colors
#'
#'@description This function maps the strata variables to colors.
#'
#'@param strata vector of stratification variables
#'@param ... further arguments to [`rand_color`][circlize::rand_color]
#'
#'@return A vector of colors.
#'
#'@author Alessandro Barberis
#'
#'
#'@keywords internal
mapStrataToColorsWithCirclize <- function(
    strata,
    ...
){

  #number of levels
  n = length(levels(factor(strata)))

  #colors
  col = circlize::rand_color(
    n = n,
    ...
  )

  return(col)
}

#'Map Strata to Colors
#'
#'@description This function maps the strata variables to colors.
#'
#'@param strata vector of stratification variables
#'@param pckg character, which R package to use for generating the colors
#'@param rcolorbrewer character, a RColorBrewer palette name. If `auto`, an
#'automatic selection based on the number of stratification variables is made
#'@param ... further arguments to [`rand_color`][circlize::rand_color]
#'
#'@return A vector of colors.
#'
#'@author Alessandro Barberis
#'
#'
#'@keywords internal
mapStrataToColors <- function(
    strata,
    pckg = c("auto","RColorBrewer", "circlize"),
    rcolorbrewer = c(
      "auto",
      #sequential palettes
      "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd",
      "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu",
      "YlOrBr", "YlOrRd",
      #qualitative palettes
      "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3",
      #divergin palettes
      "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn",
      "Spectral"
    ),
    ...
  ){
  #match
  pckg = match.arg(pckg)
  rcolorbrewer = match.arg(rcolorbrewer)

  #number of levels
  n = length(levels(factor(strata)))

  #check
  if(isTRUE(identical(pckg, "auto"))){
    if(isTRUE(n<13)){
      pckg = "RColorBrewer"
    } else {
      pckg = "circlize"
    }
  }

  #colors
  col = switch(
    pckg,
    "RColorBrewer" = mapStrataToColorsWithColorBrewer(
      strata = strata,
      palette = rcolorbrewer),
    "circlize" = mapStrataToColorsWithCirclize(strata = strata, ...)
  )

  names(col) = unique(strata)
  return(col)
}


#'Plot a `resampling` object
#'
#'@description Plot a [`resampling`] object.
#'See the **Details** section below for further information.
#'
#'@param x an object of class [`resampling`]
#'@param ... additional plot arguments
#'@param cell.width,cell.height width and height of the cells in the heatmap
#'@param col length-3 vector containing the colors for the heatmap
#'@param heatmap.legend.title character string, title of the heatmap legend
#'@inheritParams ComplexHeatmap::Heatmap
#'@param strata (optional) vector of stratification variables
#'@param strata.col (optional) vector of colors for the stratification variables
#'
#'@return A [`Heatmap-class`][ComplexHeatmap::Heatmap-class] object.
#'
#'@details This function plot the samples taken from the population as an heatmap
#'where each row corresponds to a different sample, while the columns represent
#'the elements in the population.
#'The elements belong to one of the following groups:
#'* `sample`: the sample taken from the population
#'* `holdout`: the data not selected in the sample
#'* `removed`: the data removed from the population and not considered for sampling
#'
#'If the sampling technique allows for replacement, the number of repetition are
#'shown in the cells of the heatmap.
#'
#'If `strata` is provided, an annotation is added to the heatmap.
#'
#'@examples
#'#check needed package
#'require(ComplexHeatmap, quietly = TRUE)
#'
#'#resampling object
#'x = resampling(
#'  method = 'rswor',
#'  N = 10,
#'  samples = list(c(1,5,7), c(6,2,9))
#')
#'
#'#plot
#'plot(x)
#'
#'#plot with strata
#'plot(x, strata = c(rep("a", 5), rep("b", 5)))
#'
#'@export
plot.resampling <- function(
    x,
    ...,
    cell.width  = grid::unit(10, "mm"),
    cell.height = cell.width,
    col = c("purple", "lightblue", "gray"),
    border          = NA,
    rect_gp = grid::gpar(col = "white"),
    heatmap.legend.title = "type",
    strata = NULL,
    strata.col = NULL
  ){

  #get population size
  N = getPopulationSize(x)

  #get number of samples
  k = getNumberOfSamples(x)

  #sampling method
  smid = getSamplingMethodId(x)

  #check input
  if(isFALSE(!is.null(col) && length(col)==3)){
    col = c("purple", "lightblue", "gray")
  }

  #name the vector of colors
  names(col) = c("sample", "holdout", "removed")

  #create matrix
  m = matrix(
    data = rep(x = seq(N), times = k),
    nrow = k,
    ncol = N,
    byrow = T,
    dimnames = list(
      paste0("S", seq(k)),
      seq(N)
    ))

  #get samples
  samples = getSamples(x)
  #get holdout
  holdout = getHoldOutSamples(x)
  #get removed
  removed = getRemovedElements(x)

  #update matrix
  for(i in seq(k)){
    m[i, unique(samples[[i]])] = "sample"
    if(isTRUE(length(holdout)>0)){m[i, holdout[[i]]] = "holdout"}
    if(isTRUE(length(removed)>0)){m[i, removed] = "removed"}
  }

  #check if any sample with replacement
  if(isTRUE(smid %in% listAvailableSamplingWithReplacementMethodIds())){
    #coerce
    f = lapply(X = samples, FUN = factor, levels = seq(N))
    #create contingency table
    xtab = sapply(X = f, FUN = table, simplify = T)
    #transpose
    cellann = t(xtab)
    #update
    cellann[cellann<2]=NA
  } else {
    #cell annotation
    cellann = matrix(
      data = NA,
      nrow = k,
      ncol = N,
      byrow = T,
      dimnames = list(
        paste0("S", seq(k)),
        seq(N)
      ))
  }

  #get name
  methodId = getSamplingMethodId(x)
  col_title = getSamplingMethodName(id = methodId)

  #strata
  # if(isTRUE(!is.null(strata))){
  #   hann = ComplexHeatmap::HeatmapAnnotation(
  #     df = data.frame(strata = strata),
  #     which = "column",
  #     gp = grid::gpar(by_row = T),
  #     annotation_legend_param = list(by_row = T)
  #   )
  # }

  #plot
  hm = ComplexHeatmap::Heatmap(
    matrix          = m,
    # name = title,
    # top_annotation  = hann,
    column_title    = col_title,
    border          = border,
    rect_gp         = rect_gp,
    cell_fun        = function(j,i,x,y,w,h,f){
      txt = cellann[i,j];
      if(isFALSE(is.na(txt))){
        grid::grid.text(
          label = txt,
          x = x,
          y = y,
          gp = grid::gpar(col = "white")
        )
      }
    },
    width           = ncol(m)*cell.width,
    height          = nrow(m)*cell.height,
    cluster_rows    = F,
    cluster_columns = F,
    col = col,
    heatmap_legend_param = list(
      labels = names(col),
      title = heatmap.legend.title,
      legend_gp = grid::gpar(
        fill = col,
        by_row = T
      )
    )
  )

  #strata
  if(isTRUE(!is.null(strata))){
    #set color
    if(isTRUE(is.null(strata.col))){
      strata.col = mapStrataToColors(strata = strata)
    }

    #annotation
    hann = ComplexHeatmap::HeatmapAnnotation(
      df = data.frame(strata = strata),
      which = "column",
      col = list(strata = strata.col)
    )

    # hm = ComplexHeatmap::`%v%`(x = hann, y = hm)
    hm = ComplexHeatmap::`%v%`(x = hm, y = hann)

    hann = ComplexHeatmap::HeatmapAnnotation(
      colnames = ComplexHeatmap::anno_text(x = seq(N)),
      df = NULL,
      which = "column",
      show_legend = F,
      show_annotation_name = F
    )
    hm = ComplexHeatmap::`%v%`(x = hm, y = hann)
  }

  return(hm)
}

