mae <- function(preds, obs) {
  n_out <- length(preds)
  obs_out <- tail(obs, n_out)
  mean(abs(c(preds) - obs_out))
}

rmse <- function(preds, obs) {
  n_out <- length(preds)
  obs_out <- tail(obs, n_out)  
  sqrt(mean((c(preds) - obs_out)^2))
}

pc <- function(preds, obs) {
  n_out <- length(preds)
  obs_out <- tail(obs, n_out)  
  cor(c(preds), obs_out)
}

mase <- function(preds, obs) {
  n_out <- length(preds)
  n_in <- length(obs) - n_out
  obs_out <- tail(obs, n_out)
  obs_in <- head(obs, n_in)
  
  mae_out <- mae(preds, obs_out)
  mae_in <- mae(head(obs_in, n_in - 1), tail(obs_in, n_in - 1))
  mae_out / mae_in
}

rmsse <- function(preds, obs) {
  n_out <- length(preds)
  n_in <- length(obs) - n_out  
  obs_out <- tail(obs, n_out)
  obs_in <- head(obs, n_in)
  
  rmse_out <- rmse(preds, obs_out)
  rmse_in <- rmse(head(obs_in, n_in - 1), tail(obs_in, n_in - 1))
  rmse_out / rmse_in
}

#'@export
measures <- function(preds, obs) {
  funs <- c(MAE = mae, RMSE = rmse, PC = pc, MASE = mase, RMSSE = rmsse)
  
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