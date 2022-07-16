# setClass("s_semiarma",
#   contains = "deseats",
#   slots = c(
#     decomp = "mts",
#     nonpar_model = "deseats",
#     par_model = "Arima"
#   )
# )
# 
# create_s_semiarma <- function(decomp, nonpar_model, par_model) {
#   
#   methods::new("s_semiarma",
#     decomp = decomp,
#     nonpar_model = nonpar_model,
#     par_model = par_model
#   )
#   
# }

#'@export
s_semiarma <- function(yt, smoothing_options = set_options(),
                       arma_options = list(ar_order = NULL, ma_order = NULL),
                       bwidth_start = 0.15) {
  
  ts_name <- deparse(substitute(yt))
  
  nonpar_model <- deseats(yt, smoothing_options, bwidth_start)
  
  res <- residuals(nonpar_model)
  
  ar <- arma_options$ar_order
  ma <- arma_options$ma_order
  
  if (is.null(ar) && is.null(ma)) {
    n <- length(yt)
    bic <- matrix(NA, nrow = 6, ncol = 6)
    P <- Q <- 0:5
    for (p0 in P) {
      for (q0 in Q) {
        arma <- suppressWarnings(stats::arima(res,
                                              order = c(p0, 0, q0),
                             include.mean = FALSE))
        bic[p0 + 1, q0 + 1] <- -2 * arma$loglik + (p0 + q0) * log(n)
      }
    }
    orders <- c(which(bic == min(bic), arr.ind = TRUE)) - 1
    ar <- orders[[1]]
    ma <- orders[[2]]
  } else if (is.null(ar)) {
    ar <- 0
  } else if (is.null(ma)) {
    ma <- 0
  }

  par_est <- stats::arima(res, order = c(ar, 0, ma),
                        include.mean = FALSE)

  arma_res <- par_est$residuals
  attributes(arma_res) <- attributes(nonpar_model@decomp[, 1])
  arma_fitted <- residuals(nonpar_model) - arma_res
  
  decomp <- cbind(nonpar_model@decomp, arma_fitted, arma_res)
  colnames(decomp) <- c(
    "Observations",
    "Trend",
    "Seasonality",
    "Rest",
    "Fitted ARMA",
    "Residuals ARMA"
  )
  
  out <- list(decomp = decomp, nonpar_model = nonpar_model, par_model = par_est)
  out
}