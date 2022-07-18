mu_select <- function(kernel_fun) {
  switch(kernel_fun,
    "uniform" = 0,
    "epanechnikov" = 1,
    "bisquare" = 2,
    "triweight" = 3
  )
}

bf_select <- function(boundary_method) {
  switch(boundary_method,
    "extend" = 1,
    "shorten" = 0
  )
}

setClass("deseats",
  slots = c(
    decomp = "mts",
    bwidth = "numeric",
    ts_name = "character",
    frequency = "numeric",
    order_poly = "numeric"
  )
)

create_deseats <- function(decomp, bwidth, ts_name, frequency, order_poly) {
  
  methods::new("deseats",
    decomp = decomp,
    bwidth = bwidth,
    ts_name = ts_name,
    frequency = frequency,
    order_poly = order_poly
  )
  
}

#------------------------------------------------------------------------------#

#' Locally Weighted Regression for Trend and Seasonality in Equidistant Time 
#' Series under Short Memory
#' 
#' Simultaneously estimate the trend and the 
#' seasonality via locally weighted regression in an equidistant time series 
#' under short memory. The default setting uses an iterative plug-in algorithm 
#' for identifying the asymptotically globally optimal bandwidth for smoothing.
#'
#'@param y a numerical vector or a time series object of class \code{ts} or 
#'that can be transformed with \code{\link[stats]{as.ts}} to an object of class 
#'\code{ts}; for these observations, trend and seasonality will be obtained.
#'@param smoothing_options an S4 object of class \code{smoothing_options}, which 
#'is returned by the function \code{\link{set_options}}; it 
#'includes details about the
#'options to consider in the locally weighted regression such as the order of
#'polynomial and the bandwidth for smoothing among others.
#'@param bwidth_start a single numeric value that is only relevant if the slot
#'\code{bwidth} in \code{smoothing_options} is set to \code{NA}; 
#'as the bandwidth will then
#'be selected automatically, \code{bwidth_start} sets the initial bandwidth for 
#'the algorithm.
#'
#'@export
#'
#'@details
#'
#'Work in progress. Details will be added in the release version of the package.
#'
#'@return
#'
#'The function returns and S4 object with the following elements (access them 
#'via \code{@}):
#'
#'@author
#'\itemize{
#'\item Dominik Schulz (Research Assistant) (Department of Economics, Paderborn
#'University), \cr
#'Author and Package Creator
#'\item Yuanhua Feng (Department of Economics, Paderborn
#'University), \cr
#'Author
#'}
#'
#'@examples
#'
#'Xt <- log(EXPENDITURES)
#'est <- deseats(Xt)
#'est
#'

deseats <- function(y, smoothing_options = set_options(), bwidth_start = 0.1) {

  ts_name <- deparse(substitute(y))
  
  y <- stats::as.ts(y)
  att <- attributes(y)
  
  if (is.na(smoothing_options@season)) {
    if (inherits(y, c("ts", "zoo", "xts"))) {
      s <- stats::frequency(y)
    } else {
      s <- 1
    }
  } else {
    s <- smoothing_options@season
  }
  
  # Number of observations
  n <- length(y)           

  # Select numeric values from character input using helper functions
  bf <- bf_select(smoothing_options@boundary_method)
  mu <- mu_select(smoothing_options@kernel_fun)  
  p <- smoothing_options@order_poly
  bwidth <- smoothing_options@bwidth
  if (is.na(bwidth)) {
    bwidth <- algorithmCpp(y, p, s, mu, bwidth_start)
  }

  est <- fitteddeseatsCpp(y, p, s, mu, bwidth, bf)
  Trend <- est[[2]]
  Season <- est[[3]]
  Residuals <- est[[4]]
  attributes(Trend) <- att
  attributes(Season) <- att
  attributes(Residuals) <- att
  decomp <- cbind(
    Observations = y,
    Trend = Trend,
    Season = Season,
    Rest = Residuals
  )
  
  out <- create_deseats(
    decomp = decomp,
    bwidth = bwidth,
    ts_name = ts_name,
    frequency = s,
    order_poly = p
  )
  
  out
}

### Base R plot method

#'Plot Method for Smoothing Results in the Style of Base R Plots
#'
#'This is method for producing various plots of the smoothing results
#'returned by \code{\link{deseats}}.
#'
#'@param x an object returned by the function \code{\link{deseats}}.
#'@param which various plots can be selected either via a keyword or a number; 
#'enter \code{"facets"} or \code{1} to show a facet plot of the estimated
#'time series components; enter \code{"observations"} or \code{2} to show the
#'input time series; enter \code{"fitted"} or \code{3} to show the observations 
#'alongside the estimated trend with seasonality; enter \code{"residuals"} or 
#'\code{4} to plot the both detrended and seasonally adjusted series; the 
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

setMethod("plot", "deseats", function(x, which = NULL, ...) {
  
  which <- check_which(which)
  
  if (which == "0") {
    return(invisible(NULL))
  }
  
  plot_fun <- switch(
    which,
    "facets" = plot_facets,
    "observations" = plot_obs,
    "fitted" = plot_fit,
    "residuals" = plot_resid,
    "1" = plot_facets,
    "2" = plot_obs,
    "3" = plot_fit,
    "4" = plot_resid    
  )
  
  plot_fun(x, ...)
  
})

set_default <- function(obj, repl) {
  arg_names <- names(repl)
  for (i in seq_along(arg_names)) {
    if (is.null(obj[[arg_names[[i]]]])) {
      obj[[arg_names[[i]]]] <- repl[[i]]
    }
  }
  obj
}

# Plot subfunctions

plot_facets <- function(obj, ...) {
  dots <- list(...)
  defaults <- list(
    xlab = "Time",
    main = paste0('Decomposition of the time series "', obj@ts_name,'"')
  )
  dots <- set_default(dots, defaults)
  dots[["x"]] <- obj@decomp[, 1:4]
  
  do.call(stats::plot.ts, args = dots)
}

plot_resid <- function(obj, ...) {
  dots <- list(...)
  defaults <- list(
    xlab = "Time",
    main = paste0('The estimated rest in the time series "', obj@ts_name, '"'),
    ylab = "Residual value"
  )
  dots <- set_default(dots, defaults)
  dots[["x"]] <- residuals(obj)
  
  do.call(stats::plot.ts, args = dots)  
}

plot_fit <- function(obj, ...) {
  dots <- list(...)
  defaults <- list(
    xlab = "Time",
    main = paste0('The time series "', obj@ts_name, 
                  '" together with estimated trend and seasonality'),
    ylab = obj@ts_name,
    col = c("grey74", "red"),
    lty = c(1, 1),
    type = "ll"
  )
  dots <- set_default(dots, defaults)
  dots[["x"]] <- c(stats::time(obj@decomp))
  dots[["y"]] <- cbind(obj@decomp[, 1], fitted(obj))
  
  do.call(graphics::matplot, args = dots)  
}

plot_obs <- function(obj, ...) {
  dots <- list(...)
  defaults <- list(
    xlab = "Time",
    main = paste0('The observed time series "', obj@ts_name, '"'),
    ylab = obj@ts_name
  )
  dots <- set_default(dots, defaults)
  dots[["x"]] <- obj@decomp[, 1]
  
  do.call(stats::plot.ts, args = dots)  
}

### ggplot2 plot method

#'Plot Method for Smoothing Results in the Style of ggplot2
#'
#'This is method for producing various plots of the smoothing results
#'returned by \code{\link{deseats}}.
#'
#'@param object an object returned by the function \code{\link{deseats}}.
#'@param which various plots can be selected either via a keyword or a number; 
#'enter \code{"facets"} or \code{1} to show a facet plot of the estimated
#'time series components; enter \code{"observations"} or \code{2} to show the
#'input time series; enter \code{"fitted"} or \code{3} to show the observations 
#'alongside the estimated trend with seasonality; enter \code{"residuals"} or 
#'\code{4} to plot the both detrended and seasonally adjusted series; the 
#'default is \code{which = NULL} which then lets you select a plot 
#'interactively in the R console.
#'@param ... no purpose and only implemented for compatibility.
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
#'autoplot(est, which = 3)
#'

setMethod("autoplot", "deseats", function(object, which = NULL, ...) {
  
  which <- check_which(which)
  
  if (which == "0") {
    return(invisible(NULL))
  }
  
  plot_fun <- switch(
    which,
    "facets" = plot_facets_gg,
    "observations" = plot_obs_gg,
    "fitted" = plot_fit_gg,
    "residuals" = plot_resid_gg,
    "1" = plot_facets_gg,
    "2" = plot_obs_gg,
    "3" = plot_fit_gg,
    "4" = plot_resid_gg    
  )
  
  plot_fun(object, ...)
  
})


# Plot subfunctions

plot_facets_gg <- function(obj, ...) {
  
    c_names <- colnames(obj@decomp)[1:4]

    n <- length(obj@decomp[, 1])
    t <- c(stats::time(obj@decomp))

    df <- data.frame(
      Time = rep(t, 4),
      Value = c(as.numeric(obj@decomp[, 1]), as.numeric(obj@decomp[, 2]), as.numeric(obj@decomp[, 3]), as.numeric(obj@decomp[, 4])),
      Series = factor(c(rep(1:4, each = n)), labels = c_names)
    )
    ggplot2::ggplot(df, ggplot2::aes(x = Time, y = Value)) +
      ggplot2::geom_line() +
      ggplot2::ggtitle(paste0('Decomposition of the time series "', obj@ts_name, '"')) +
      ggplot2::xlab("Time") +
      ggplot2::facet_grid(Series ~ ., scales = "free_y") +
      ggplot2::theme(axis.title.y = ggplot2::element_blank())

}

plot_resid_gg <- function(obj, ...) {
  df <- data.frame(
    Time = c(stats::time(obj@decomp)),
    Value = c(residuals(obj))
  )

  ggplot2::ggplot(df, ggplot2::aes(x = Time, y = Value)) +
    ggplot2::geom_line() +
    ggplot2::ylab("Residual value") +
    ggplot2::ggtitle(paste0('The estimated rest in the time series "', obj@ts_name, '"')) 
}

plot_fit_gg <- function(obj, ...) {

    n <- length(obj@decomp[, 1])
    t <- c(stats::time(obj@decomp))

    df <- data.frame(
      Time = rep(t, 2),
      Value = c(as.numeric(obj@decomp[, 1]), as.numeric(fitted(obj))),
      Series = factor(c(rep(1:2, each = n)), labels = c("Observations", "Fitted values"))
    )
    ggplot2::ggplot(df, ggplot2::aes(x = Time, y = Value)) +
      ggplot2::geom_line(ggplot2::aes(color = Series)) +
      ggplot2::ggtitle(paste0('The time series "', obj@ts_name, 
                  '" together with estimated trend and seasonality')) +
      ggplot2::ylab(obj@ts_name) +
        ggplot2::scale_color_manual(values = c("grey74", "red"))
}

plot_obs_gg <- function(obj, ...) {
  df <- data.frame(
    Time = c(stats::time(obj@decomp)),
    Value = c(obj@decomp[, c(1)])
  )

  ggplot2::ggplot(df, ggplot2::aes(x = Time, y = Value)) +
    ggplot2::geom_line() +
    ggplot2::ylab(obj@ts_name) +
    ggplot2::ggtitle(paste0('The observed time series "', obj@ts_name, '"')) 
}

##--------------

check_which <-function(which) {
  
    if (is.null(which)) {
    text_prompt <- data.frame(
      c("facets:", "observations:", "fitted:", "residuals:"),
      c(
        "Facet plot of the estimated components",
        "The observed time series",
        "The observations together with estimated trend + seasonality",
        "The both detrended and seasonally adjusted series"
      )
    )
    colnames(text_prompt) <- NULL
    
    cat("\nSelect one of the following plots via the keyword or the position number (exit with 0):\n")
    
    print.data.frame(text_prompt, row.names = FALSE, right = FALSE)
    
    cat("\n")
    
    which <- ""
    
    while(!(which %in% c(0, 1, 2, 3, 4, "facets", "observations", "fitted",
                         "residuals"))) {
    
        which <- readline("Keyword or position number: ")
        
    }
    cat("\n")
    }
  
    which
}

#------------

#'@export
setMethod("show", "deseats",
  function(object){
    cat(paste0("\n",
      "************************************************\n",
      "*                                              *\n",
      "*               Smoothing Results              *\n",
      "*                                              *\n",
      "************************************************\n\n",
      " Series: ", object@ts_name, "\n",
      " Estimated: Trend + Seasonality\n",
      " Bandwidth: ", sprintf("%.4f", object@bwidth), "\n\n",      
      " Trend:\n",
      " ------\n",
      "   Order of local polynomial: ", object@order_poly, "\n\n",
      " Seasonality:\n",
      " ------------\n",
      "   Frequency: ", object@frequency, "\n\n"
    ))
  }
)

#----------------

#'@export
methods::setGeneric("trend", function(x) standardGeneric("trend"))
#'@export
methods::setMethod("trend", "deseats", function(x) x@decomp[, 2])
#'@export
methods::setMethod("season", "deseats", function(x) x@decomp[, 3])
