setClass("deseats_fit",
  slots = c(
    fit = "list",
    print = "character"
  ),
  prototype = list(
    fit = list(
      trend = NULL,
      season = NULL,
      trend_season = NULL,
      residuals = NULL,
      cf0 = NULL,
      bandwidth_opt = NULL,
      nobs = NULL
    ),
    print = NA_character_
  )
)

set_DeSeaTS <- function(
  fit = list(
    trend = NULL, season = NULL, trend_season = NULL, 
    residuals = NULL, cf0 = NULL, bandwidth_applied = NULL, bandwidth_opt = NULL, 
    nobs = NULL
  ), print = NA_character_) {
  
  new("deseats_fit",
    fit = fit,
    print = print
  )
  
}

setClass("deseats_bwdith",
  slots = c(
    specs = "list",
    bandwidth_opt = "numeric"
  ),
  prototype = list(
    specs = list(
      order_poly = 1,
      period = NULL,
      kernel_fun = "Epanechnikov"
    ),
    bandwidth_opt = 0.15
  )
)

set_deseats_bwidth <- function(
  specs = list(
    order_poly = 1,
    period = NULL,
    kernel_fun = "Epanechnikov"
  ), bandwidth_opt = 0.15) {
  
  new("deseats_bwidth",
      specs = specs,
      bandwidth_opt = bandwidth_opt
  )
}

methods::setClass("deseats_spec",
  slots = c(
    order_poly = "numeric",
    period = "numeric",
    kernel_fun = "character",
    bandwidth = "numeric"
  ),
  prototype = list(
    order_poly = 1,
    period = NA_real_,
    kernel_fun = "Epanechnikov",
    bandwidth = 0.15
  )
)

deseats_spec <- function(order_poly = 1, period = NA_real_, 
  kernel_fun = "Epanechnikov", bandwidth = 0.15) {
  
  methods::new("deseats_spec",
    order_poly = order_poly,
    period = period,
    kernel_fun = kernel_fun,
    bandwidth = bandwidth
  )
}

methods::setGeneric("order_poly", function(x) standardGeneric("order_poly"))
methods::setGeneric("order_poly<-", function(x, value) standardGeneric("order_poly<-"))
methods::setMethod("order_poly", "deseats_spec", function(x) x@order_poly)
methods::setMethod("order_poly<-", "deseats_spec", function(x, value) {
  x@order_poly <- value
  x
})


methods::setGeneric("period", function(x) standardGeneric("period"))
methods::setGeneric("period<-", function(x, value) standardGeneric("period<-"))

methods::setMethod("period", "deseats_spec", function(x) x@period)
methods::setMethod("period<-", "deseats_spec", function(x, value) {
  x@period <- value
  x
})

methods::setGeneric("kernel_fun", function(x) standardGeneric("kernel_fun"))
methods::setGeneric("kernel_fun<-", function(x, value) standardGeneric("kernel_fun<-"))

methods::setMethod("kernel_fun", "deseats_spec", function(x) x@kernel_fun)
methods::setMethod("kernel_fun<-", "deseats_spec", function(x, value) {
  x@kernel_fun <- value
  x
})

methods::setGeneric("bandwidth", function(x) standardGeneric("bandwidth"))
methods::setGeneric("bandwidth<-", function(x, value) standardGeneric("bandwidth<-"))

methods::setMethod("bandwidth", "deseats_spec", function(x) x@bandwidth)
methods::setMethod("bandwidth<-", "deseats_spec", function(x, value) {
  x@bandwidth <- value
  x
})

