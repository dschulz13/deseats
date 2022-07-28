setClass("bv41_arma",
  slots = c(
    decomp = "mts",
    nonpar_model = "bv41",
    par_model = "Arima",
    ts_name = "character"
  )
)

create_bv41_arma <- function(decomp, nonpar_model, par_model, ts_name) {

  methods::new("bv41_arma",
    decomp = decomp,
    nonpar_model = nonpar_model,
    par_model = par_model,
    ts_name = ts_name
  )

}

#'@export
setMethod("show", "bv41_arma", function(object) {
  
  invisible(NULL)
  
})

#'@export
setMethod("predict", "bv41_arma", function(object, n.ahead = 1, ...) {
  
  .f <- methods::selectMethod("predict", "s_semiarma")
  .f(object, n.ahead, ...)  
  
})


#'@export
setMethod("trend", "bv41_arma", function(x) {
  
  .f <- methods::selectMethod("trend", "deseats")
  .f(x)
  
})

#'@export
setMethod("season", "bv41_arma", function(x) {
  
  .f <- methods::selectMethod("season", "deseats")
  .f(x)
  
})

#'@export
setMethod("fitted", "bv41_arma", function(object, ...) {
  
  .f <- methods::selectMethod("fitted", "deseats")
  .f(object)
  
})

#'@export
setMethod("residuals", "bv41_arma", function(object, ...) {
  
  .f <- methods::selectMethod("residuals", "deseats")
  .f(object)
  
})

#'@export
setMethod("deseasonalize", "bv41_arma",
  function(x, ...){
    .f <- methods::selectMethod("deseasonalize", "deseats")
    .f(x)
  }
)

#'@export
setMethod("detrend", "bv41_arma",
  function(x, ...){
    .f <- methods::selectMethod("detrend", "deseats")
    .f(x)
  }
)

#'@export
setMethod("plot", "bv41_arma", function(x, which = NULL, ...) {
  
  .f <- methods::selectMethod("plot", "deseats")
  
  .f(x, which = which, ...)
  
})

#'@export
setMethod("autoplot", "bv41_arma", function(object, which = NULL, ...) {
  
  .f <- methods::selectMethod("autoplot", "deseats")
  
  .f(object, which = which, ...)
  
})
