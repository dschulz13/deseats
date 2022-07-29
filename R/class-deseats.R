

setClass("deseats",
  slots = c(
    decomp = "mts",
    bwidth = "numeric",
    ts_name = "character",
    frequency = "numeric",
    order_poly = "numeric",
    weights = "array",
    weights_wfun = "matrix",
    boundary_method = "character",
    kernel_fun = "character"
  )
)

create_deseats <- function(decomp, bwidth, ts_name, frequency, order_poly,
                           weights, weights_wfun, boundary_method,
                           kernel_fun) {
  
  methods::new("deseats",
    decomp = decomp,
    bwidth = bwidth,
    ts_name = ts_name,
    frequency = frequency,
    order_poly = order_poly,
    weights = weights,
    weights_wfun = weights_wfun,
    boundary_method = boundary_method,
    kernel_fun = kernel_fun
  )
  
}

#------------------------------------------------------------------------------#

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
    "deseasonalized" = plot_deseason,
    "detrended" = plot_detrend,    
    "1" = plot_facets,
    "2" = plot_obs,
    "3" = plot_fit,
    "4" = plot_resid,
    "5" = plot_deseason,
    "6" = plot_detrend   
  )
  
  plot_fun(x, ...)
  
})





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
    "deseasonalized" = plot_deseason_gg,
    "detrended" = plot_detrend_gg,    
    "1" = plot_facets_gg,
    "2" = plot_obs_gg,
    "3" = plot_fit_gg,
    "4" = plot_resid_gg,
    "5" = plot_deseason_gg,
    "6" = plot_detrend_gg     
  )
  
  plot_fun(object, ...)
  
})

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
methods::setMethod("trend", "deseats", function(x) x@decomp[, 2])
#'@export
methods::setMethod("season", "deseats", function(x) x@decomp[, 3])

#'@export
setMethod("fitted", "deseats",
  function(object, ...){
    object@decomp[, 2] + object@decomp[, 3]
  }
)

#'@export
setMethod("residuals", "deseats",
  function(object, ...){
    object@decomp[, 4]
  }
)

#'@export
setMethod("deseasonalize", "deseats",
  function(x, ...){
    x@decomp[, 1] - season(x)
  }
)

#'@export
setMethod("detrend", "deseats",
  function(x, ...){
    x@decomp[, 1] - trend(x)
  }
)

#-----------------

#'@export
methods::setMethod("animate", "deseats", 
function(object, col.obs = "grey74", col.fit = "red", 
                    col.weights = "#00D40E", col.window = "deepskyblue4", 
                    col.spot = "orange", save = NULL,
                    xlab = "Time", ylab1 = "Estimated trend + seasonality",
                    ylab2 = "Active kernel weights",
                    main = NULL, ...) {
  
  n <- length(object@decomp[, 1])                  # Number of observations
  habs <- (length(object@weights[, 1, "Trend"]) - 1) / 2  # Number of boundary points 
  
  t <- c(stats::time(object@decomp))
  tsub <- 1:n
  
  oldPar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldPar), add = TRUE)
  
  parNew <- list(
    mar = c(5, 4, 4, 4) + 0.1
  )

  d <- abs(diff(range(object@decomp[, 1])))
  min_y <- min(object@decomp[, 1])
  max_y <- max(object@decomp[, 1])  
  
  mat_reg <- as.data.frame(matrix(0, ncol = object@order_poly + object@frequency,
                           nrow = 2 * habs + 1))
  names(mat_reg) <- c("y", paste0("t", 1:object@order_poly), 
                      paste0("cos", 1:trunc((object@frequency - 1) / 2)),
                      paste0("sin", 1:trunc((object@frequency - 1) / 2)),
                      paste0("cos", (trunc(object@frequency - 1) / 2) + 1))
  mat_reg[["y"]] <- object@decomp[1:(2 * habs + 1), 1]
  
  ani_opts_old <- animation::ani.options()
  on.exit(animation::ani.options(ani_opts_old), add = TRUE)
  animation::ani.options(nmax = 2 * (habs) + 7 * 10, interval = 0.1, ani.width = 1200)  
    
  if (is.null(main)) {
    main <- paste0('Locally weighted regression illustrated for the series "', 
                       object@ts_name, '"')
  }
  
  .f <- function() {
  
  graphics::par(parNew)    
    
  for (frame in seq_len(habs)) {
    
    tt <- (tsub[1:(2 * habs + 1)] - tsub[[frame]]) / habs
    for (i in 1:object@order_poly) {
      mat_reg[[paste0("t", i)]] <- tt^i
    }
    for (i in 1:((trunc(object@frequency - 1) / 2))) {
      mat_reg[[paste0("cos", i)]] <- cos(2 * i * pi * tt / (object@frequency / habs))
      mat_reg[[paste0("sin", i)]] <- sin(2 * i * pi * tt / (object@frequency / habs))      
    }
    mat_reg[[paste0("cos", trunc(object@frequency - 1) / 2 + 1)]] <- cos(pi * tt * habs)
    
    lm_fit <- (stats::lm(y ~ ., data = mat_reg, weights = object@weights_wfun[frame, ]))$fitted.values
    
    plot(object@decomp[, 1], col = col.obs,
         main = main, ylab = "", xlab = xlab)
    graphics::axis(side = 2, col = col.fit, col.ticks = col.fit, col.axis = col.fit)
    graphics::mtext(ylab1, side = 2, line = 3, col = col.fit)      
    graphics::polygon(c(rep(t[[1]], 2), rep(t[[2 * habs + 1]], 2)), 
            c(min_y - 0.1 * d, max_y + 0.1 * d, max_y + 0.1 * d, min_y - 0.1 * d),
            border = NA, col = ggplot2::alpha(col.window, 0.1))
    graphics::abline(v = c(t[[1]], t[[2 * habs + 1]]), col = col.window, lwd = 2)
    graphics::abline(v = t[[frame]], col = col.spot, lty = "dashed", lwd = 2)
    graphics::lines(t[1:frame], fitted(object)[1:frame], col = col.fit, lwd = 1)    
    graphics::lines(t[1:(2 * habs + 1)], lm_fit, col = ggplot2::alpha("black", 0.9), lwd = 1, lty = 3)
    graphics::points(t[[frame]], fitted(object)[[frame]], col = ggplot2::alpha("purple", 0.4), pch = 19, cex = 1.2)    
    
    graphics::par(new = TRUE)                             # Add new plot
    plot(0,                                     # Create second plot without axes
    axes = FALSE, xlab = "", ylab = "", type = "n", xlim = c(min(t), max(t)),
    ylim = c(min(object@weights[, , "Combined"]), max(object@weights[, , "Combined"])))
    graphics::abline(h = 0, col = "lightgray", lty = "dotted")
    graphics::lines(t[1:(2 * habs + 1)], object@weights[frame, , "Combined"], col = ggplot2::alpha(col.weights, 0.9))
    graphics::axis(side = 4, col = col.weights, col.ticks = col.weights,
         col.axis = col.weights)       # Add second axis
    graphics::mtext(ylab2, side = 4, line = 3, col = col.weights) 
    
    animation::ani.pause()
  }
  
  t_in <- floor(seq(from = habs + 1, to = n - habs, length.out = 6))
  tt <- (tsub[1:(2 * habs + 1)] - tsub[[habs + 1]])  / habs  
  
  for (i in 1:object@order_poly) {
    mat_reg[[paste0("t", i)]] <- tt^i
  }
  for (i in 1:((trunc(object@frequency - 1) / 2))) {
    mat_reg[[paste0("cos", i)]] <- cos(2 * i * pi * tt / (object@frequency / habs))
    mat_reg[[paste0("sin", i)]] <- sin(2 * i * pi * tt / (object@frequency / habs))      
  }
  mat_reg[[paste0("cos", trunc(object@frequency - 1) / 2 + 1)]] <- cos(pi * tt * habs)
  
  counter <- 0
  
  for (frame in t_in) {
    
    for(re in 1:10){
    
    counter <- counter + 1  
      
    mat_reg[["y"]] <- object@decomp[(frame - habs):(frame + habs), 1]
    
    lm_fit <- (stats::lm(y ~ ., data = mat_reg, weights = object@weights_wfun[habs + 1, ]))$fitted.values    
    
    plot(object@decomp[, 1], col = col.obs,
         main = main, 
         ylab = "", xlab = xlab)
    graphics::points(0, counter, type = "n")    
    graphics::axis(side = 2, col = col.fit, col.ticks = col.fit, col.axis = col.fit)
    graphics::mtext(ylab1, side = 2, line = 3, col = col.fit)      
    graphics::polygon(c(rep(t[[frame - habs]], 2), rep(t[[frame + habs]], 2)), 
            c(min_y - 0.1 * d, max_y + 0.1 * d, max_y + 0.1 * d, min_y - 0.1 * d),
            border = NA, col = ggplot2::alpha(col.window, 0.1))
    graphics::abline(v = c(t[[frame - habs]], t[[frame + habs]]), col = col.window, lwd = 2)
    graphics::abline(v = t[[frame]], col = col.spot, lty = "dashed", lwd = 2)
    graphics::lines(t[1:frame], fitted(object)[1:frame], col = col.fit, lwd = 1)
    graphics::lines(t[(frame - habs):(frame + habs)], lm_fit, col = ggplot2::alpha("black", 0.9), lwd = 1, lty = 3)    
    graphics::points(t[[frame]], fitted(object)[[frame]], col = ggplot2::alpha("purple", 0.4), pch = 19, cex = 1.2)
    
    graphics::par(new = TRUE)                             # Add new plot
    plot(0,                                     # Create second plot without axes
    axes = FALSE, xlab = "", ylab = "", type = "n", xlim = c(min(t), max(t)),
    ylim = c(min(object@weights[, , "Combined"]), max(object@weights[, , "Combined"])))
    graphics::abline(h = 0, col = "lightgray", lty = "dotted")
    
    graphics::lines(t[(frame - habs):(frame + habs)], object@weights[habs + 1, , "Combined"], col = ggplot2::alpha(col.weights, 0.9))
    graphics::axis(side = 4, col = col.weights, col.ticks = col.weights,
         col.axis = col.weights)       # Add second axis
    graphics::mtext(ylab2, side = 4, line = 3, col = col.weights) 
    animation::ani.pause()    
    
    }
    
  }
    
  mat_reg[["y"]] <- object@decomp[(n - 2 * habs):n, 1]
    
  for (frame in c((n - habs + 1):n, rep(n, 10))) {
    
    counter <- counter + 1
    
    tt <- (tsub[(n - 2 * habs):n] - tsub[[frame]]) / habs
    for (i in 1:object@order_poly) {
      mat_reg[[paste0("t", i)]] <- tt^i
    }
    for (i in 1:((trunc(object@frequency - 1) / 2))) {
      mat_reg[[paste0("cos", i)]] <- cos(2 * i * pi * tt / (object@frequency / habs))
      mat_reg[[paste0("sin", i)]] <- sin(2 * i * pi * tt / (object@frequency / habs))      
    }
    mat_reg[[paste0("cos", trunc(object@frequency - 1) / 2 + 1)]] <- cos(pi * tt * habs)
    
    lm_fit <- (stats::lm(y ~ ., data = mat_reg, weights = object@weights_wfun[frame + 2 * habs - n + 1, ]))$fitted.values
    
    plot(object@decomp[, 1], col = col.obs,
         main = main, 
         ylab = "", xlab = xlab)
    graphics::points(0, counter, type = "n")
    graphics::axis(side = 2, col = col.fit, col.ticks = col.fit, col.axis = col.fit)
    graphics::mtext(ylab1, side = 2, line = 3, col = col.fit)      
    graphics::polygon(c(rep(t[[n - 2 * habs]], 2), rep(t[[n]], 2)), 
            c(min_y - 0.1 * d, max_y + 0.1 * d, max_y + 0.1 * d, min_y - 0.1 * d),
            border = NA, col = ggplot2::alpha(col.window, 0.1))
    graphics::abline(v = c(t[[n - 2 * habs]], t[[n]]), col = col.window, lwd = 2)
    graphics::abline(v = t[[frame]], col = col.spot, lty = "dashed", lwd = 2)
    graphics::lines(t[1:frame], fitted(object)[1:frame], col = col.fit, lwd = 1)
    graphics::lines(t[(n - 2 * habs):n], lm_fit, col = ggplot2::alpha("black", 0.9), lwd = 1, lty = 3)    
    graphics::points(t[[frame]], fitted(object)[[frame]], col = ggplot2::alpha("purple", 0.4), pch = 19, cex = 1.2)
    
    graphics::par(new = TRUE)                             # Add new plot
    plot(0,                                     # Create second plot without axes
    axes = FALSE, xlab = "", ylab = "", type = "n", xlim = c(min(t), max(t)),
    ylim = c(min(object@weights[, , "Combined"]), max(object@weights[, , "Combined"])))
    graphics::abline(h = 0, col = "lightgray", lty = "dotted")
    
    graphics::lines(t[(n - 2 * habs):n], object@weights[frame + 2 * habs - n + 1, , "Combined"], col = ggplot2::alpha(col.weights, 0.9))
    graphics::axis(side = 4, col = col.weights, col.ticks = col.weights,
         col.axis = col.weights)       # Add second axis
    graphics::mtext(ylab2, side = 4, line = 3, col = col.weights) 
    animation::ani.pause()
  }
  
  }

if (is.null(save)) {
  .f()
} else {
  .savefun <- aniSave(save)
  .savefun({.f()}, interval = 0.1, ani.width = 1200)
}
}
)

#--------------------------------------#

setMethod(".boot_f_generate", "deseats",
  function(object, ...){
    fitted_vals <- fitted(object)
    resids <- residuals(object)
    n <- length(fitted_vals)
    opts <- set_options(
      order_poly = object@order_poly, season = object@frequency,
      kernel_fun = object@kernel_fun,
      boundary_method = object@boundary_method
    )
    
    l <- 10
    
    m <- trunc(n / l) + 1 * (n / l != trunc(n / l))
    
    k <- n - l + 1
    
    blocks <- lapply(
      1:k,
      function(.x, Xt, l) {
        
        Xt[(.x):(.x + l - 1)]
        
      }, Xt = resids, l = l
    )
    
    function(zt) {
      Zt <- fitted_vals + 
        head(unname(unlist(sample(blocks, size = m, replace = TRUE))), n)
      deseats(Zt, opts, bwidth_start = 0.1)@bwidth
    }
    
  }
)

#'@export
setMethod("bwidth_interval", "deseats",
  function(object, ...){
    .fun <- .boot_f_generate(object)
    
    ncores <- future::availableCores() - 1
    oldPlan <- future::plan()
    on.exit(future::plan(oldPlan), add = TRUE, after = TRUE)
    future::plan(future::multisession)
    bwidths <- furrr::future_map_dbl(
      1:1000,
      .f = .fun,
      .options = furrr::furrr_options(seed = TRUE),
      .progress = TRUE
    )
    
    cat("\n")
    
    q <- stats::quantile(bwidths, probs = c(0.025, 0.975))
    q_names <- names(q)
    out <- c(q[[1]], object@bwidth, q[[2]])
    names(out) <- c(q_names[[1]], "Estimate", q_names[[2]])
    out
  }
)
