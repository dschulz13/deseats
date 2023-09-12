setClass("decomp",
  slots = c(
    decomp = "mts",
    ts_name = "character",
    frequency = "numeric"
  )
)

#------------------------------------------------------------------------------#

### Base R plot method

#'Plot Method for Decomposition Results in the Style of Base R Plots
#'
#'This is method for producing various plots of the decomposition results
#'returned by this package.
#'
#'@param x an object returned by the function \code{\link{deseats}}.
#'@param which various plots can be selected either via a keyword or a number; 
#'enter \code{"facets"} or \code{1} to show a facet plot of the estimated
#'time series components; enter \code{"observations"} or \code{2} to show the
#'input time series; enter \code{"fitted"} or \code{3} to show the observations 
#'alongside the estimated trend with seasonality; enter \code{"residuals"} or 
#'\code{4} to plot the both detrended and seasonally adjusted series; use 
#'\code{5} or \code{"deseasonalized"} to show the seasonally adjusted 
#'series; enter \code{6} or \code{"detrended"} to plot the detrended 
#'series; the 
#'default is \code{which = NULL} which then lets you select a plot 
#'interactively in the R console.
#'@param ... further arguments to pass to \code{\link[stats]{plot.ts}} or 
#'\code{\link[graphics]{matplot}} (depending on whether only one time series
#'or multiple time series are shown in the plot).
#'
#'@details
#'
#'Work in progress. Details will be added soon.
#'
#'@importFrom ggplot2 autoplot
#'@export 
#'
#'@return
#'None
#'
#'@author
#'\itemize{
#'\item Dominik Schulz (Research Assistant) (Department of Economics, Paderborn
#'University), \cr
#'Author and Package Creator
#'}
#'
#'@examples
#'
#'Xt <- log(EXPENDITURES)
#'est <- deseats(Xt)
#'plot(est, which = 3)
#'

setMethod("plot", "decomp", function(x, which = NULL, ...) {
  
  which <- check_which(which)
  
  if (which == "0") {
    return(invisible(NULL))
  }
  
  plot_fun <- switch(
    which,
    "facets" = plot_facets,
    "observations" = plot_obs,
    "fitted" = plot_fit,
    "detailed_fit" = plot_fit_detailed,
    "trend_season" = plot_components,
    "residuals" = plot_resid,
    "deseasonalized" = plot_deseason,
    "detrended" = plot_detrend,    
    "1" = plot_facets,
    "2" = plot_obs,
    "3" = plot_fit,
    "4" = plot_fit_detailed,
    "5" = plot_components,
    "6" = plot_resid,
    "7" = plot_deseason,
    "8" = plot_detrend   
  )
  
  plot_fun(x, ...)
  
})


### ggplot2 plot method

#'Plot Method for Decomposition Results in the Style of ggplot2
#'
#'This is method for producing various plots of the decomposition results
#'returned by this package.
#'
#'@param object an object returned by the function \code{\link{deseats}}.
#'@param which various plots can be selected either via a keyword or a number; 
#'enter \code{"facets"} or \code{1} to show a facet plot of the estimated
#'time series components; enter \code{"observations"} or \code{2} to show the
#'input time series; enter \code{"fitted"} or \code{3} to show the observations 
#'alongside the estimated trend with seasonality; enter \code{"residuals"} or 
#'\code{4} to plot the both detrended and seasonally adjusted series; use 
#'\code{5} or \code{"deseasonalized"} to show the seasonally adjusted 
#'series; enter \code{6} or \code{"detrended"} to plot the detrended 
#'series; the 
#'default is \code{which = NULL} which then lets you select a plot 
#'interactively in the R console.
#'@param ... no purpose and only implemented for compatibility.
#'
#'@details
#'
#'Work in progress. Details will be added soon.
#'
#'@export
#'@importFrom ggplot2 autoplot
#'
#'@return
#'None
#'
#'@author
#'\itemize{
#'\item Dominik Schulz (Research Assistant) (Department of Economics, Paderborn
#'University), \cr
#'Author and Package Creator
#'}
#'
#'@examples
#'
#'Xt <- log(EXPENDITURES)
#'est <- deseats(Xt)
#'autoplot(est, which = 3)
#'

setMethod("autoplot", "decomp", function(object, which = NULL, ...) {
  
  which <- check_which(which)
  
  if (which == "0") {
    return(invisible(NULL))
  }
  
  plot_fun <- switch(
    which,
    "facets" = plot_facets_gg,
    "observations" = plot_obs_gg,
    "fitted" = plot_fit_gg,
    "detailed_fit" = plot_fit_detailed_gg,
    "trend_season" = plot_components_gg,
    "residuals" = plot_resid_gg,
    "deseasonalized" = plot_deseason_gg,
    "detrended" = plot_detrend_gg,    
    "1" = plot_facets_gg,
    "2" = plot_obs_gg,
    "3" = plot_fit_gg,
    "4" = plot_fit_detailed_gg,
    "5" = plot_components_gg,
    "6" = plot_resid_gg,
    "7" = plot_deseason_gg,
    "8" = plot_detrend_gg     
  )
  
  P <- plot_fun(object, ...)
  P
  
})

#--------------------------------------------------------------

#'@export
methods::setMethod("trend", "decomp", function(x) {
  tryCatch({
    x@decomp[, "Trend"]
  }, error = function(c1) {stop("No trend component to retrieve.", call. = FALSE)})
  
})
#'@export
methods::setMethod("season", "decomp", function(x) {
  tryCatch({
    x@decomp[, "Season"]
  }, error = function(c1) {stop("No seasonal component to retrieve.", call. = FALSE)})
  
})

#'@export
setMethod("fitted", "decomp",
  function(object, ...){
    trend(object) + season(object)
  }
)

#'@export
setMethod("residuals", "decomp",
  function(object, ...){
    object@decomp[, "Rest"]
  }
)

#'@export
setMethod("deseasonalize", "decomp",
  function(x, ...){
    x@decomp[, "Observations"] - season(x)
  }
)

#'@export
setMethod("detrend", "decomp",
  function(x, ...){
    x@decomp[, "Observations"] - trend(x)
  }
)
