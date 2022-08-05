setClass("hfilter",
  contains = "decomp",
  slots = c(
    regression_output = "lm"
  )
)

create_hfilter <- function(decomp, ts_name, frequency, regression_output) {
  
  methods::new("hfilter",
    decomp = decomp,
    ts_name = ts_name,
    frequency = frequency,
    regression_output = regression_output
  )
  
}

#'@export
setMethod("fitted", "hfilter", function(object, ...) {
  
  na.omit(object@decomp[, "Fitted"])
  
})

#'@export
setMethod("residuals", "hfilter", function(object, ...) {
  
  na.omit(object@decomp[, "Rest"])
  
})

# Plot method

#'@export
setMethod("plot", "hfilter", function(x, which = NULL, ...) {
  
  which <- check_which_hf(which)
  
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

#'@export
setMethod("autoplot", "hfilter", function(object, which = NULL, ...) {
  
  which <- check_which_hf(which)
  
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