setClass("bv41_arma",
  contains = "decomp",
  slots = c(
    nonpar_model = "bv41",
    par_model = "Arima"
  )
)

create_bv41_arma <- function(decomp, nonpar_model, par_model) {

  methods::new("bv41_arma",
    decomp = decomp,
    nonpar_model = nonpar_model,
    par_model = par_model,
    ts_name = nonpar_model@ts_name,
    frequency = nonpar_model@frequency
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

