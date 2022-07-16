### Methods ###

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

