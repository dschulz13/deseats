setClass("lmdecomp",
  slots = c(
    decomp = "mts",
    ts_name = "character",
    frequency = "numeric",
    regression_output = "lm"
  )
)

create_lmdecomp <- function(decomp, ts_name, frequency, regression_output) {
  
  methods::new("lmdecomp",
    decomp = decomp,
    ts_name = ts_name,
    frequency = frequency,
    regression_output = regression_output
  )
  
}

#'@export
setMethod("trend", "lmdecomp", function(x) {
  
  .f <- methods::selectMethod("trend", "deseats")
  .f(x)
  
})

#'@export
setMethod("season", "lmdecomp", function(x) {
  
  .f <- methods::selectMethod("season", "deseats")
  .f(x)
  
})

#'@export
setMethod("fitted", "lmdecomp", function(object, ...) {
  
  .f <- methods::selectMethod("fitted", "deseats")
  .f(object)
  
})

#'@export
setMethod("residuals", "lmdecomp", function(object, ...) {
  
  .f <- methods::selectMethod("residuals", "deseats")
  .f(object)
  
})

#'@export
setMethod("deseasonalize", "lmdecomp",
  function(x, ...){
    .f <- methods::selectMethod("deseasonalize", "deseats")
    .f(x)
  }
)

#'@export
setMethod("detrend", "lmdecomp",
  function(x, ...){
    .f <- methods::selectMethod("detrend", "deseats")
    .f(x)
  }
)

#'@export
setMethod("plot", "lmdecomp", function(x, which = NULL, ...) {
  
  .f <- methods::selectMethod("plot", "deseats")
  
  .f(x, which = which, ...)
  
})

#'@export
setMethod("autoplot", "lmdecomp", function(object, which = NULL, ...) {
  
  .f <- methods::selectMethod("autoplot", "deseats")
  
  .f(object, which = which, ...)
  
})