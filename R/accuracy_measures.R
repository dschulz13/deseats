mae <- function(preds, obs) {
  mean(abs(preds - obs))
}

rmse <- function(preds, obs) {
  sqrt(mean((preds - obs)^2))
}


#'@export
measures <- function(preds, obs) {
  funs <- c(MAE = mae, RMSE = rmse)
  
  out <- vapply(
    X = funs,
    FUN = function(.f, preds, obs) {
      .f(preds, obs)
    },
    FUN.VALUE = numeric(1),
    preds = preds,
    obs = obs,
    USE.NAMES = TRUE
  )
  out
}