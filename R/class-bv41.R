setClass("bv41",
  slots = c(
    decomp = "mts",
    ts_name = "character",
    frequency = "numeric"
  )
)

create_bv41 <- function(decomp, ts_name, frequency) {
  
  methods::new("bv41",
    decomp = decomp,
    ts_name = ts_name,
    frequency = frequency
  )
  
}

#'@export
setMethod("trend", "bv41", function(x) {
  
  .f <- methods::selectMethod("trend", "deseats")
  .f(x)
  
})

#'@export
setMethod("season", "bv41", function(x) {
  
  .f <- methods::selectMethod("season", "deseats")
  .f(x)
  
})

#'@export
setMethod("fitted", "bv41", function(object, ...) {
  
  .f <- methods::selectMethod("fitted", "deseats")
  .f(object)
  
})

#'@export
setMethod("residuals", "bv41", function(object, ...) {
  
  .f <- methods::selectMethod("residuals", "deseats")
  .f(object)
  
})

#'@export
setMethod("plot", "bv41", function(x, which = NULL, ...) {
  
  .f <- methods::selectMethod("plot", "deseats")
  
  .f(x, which = which, ...)
  
})

#'@export
setMethod("autoplot", "bv41", function(object, which = NULL, ...) {
  
  .f <- methods::selectMethod("autoplot", "deseats")
  
  .f(object, which = which, ...)
  
})

