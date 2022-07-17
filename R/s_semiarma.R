setOldClass("Arima")

setClass("s_semiarma",
  contains = c("deseats", "Arima"),
  slots = c(
    decomp = "mts",
    nonpar_model = "deseats",
    par_model = "Arima",
    ts_name = "character"
  )
)

create_s_semiarma <- function(decomp, nonpar_model, par_model, ts_name) {

  methods::new("s_semiarma",
    decomp = decomp,
    nonpar_model = nonpar_model,
    par_model = par_model,
    ts_name = ts_name
  )

}

#'@export
s_semiarma <- function(yt, smoothing_options = set_options(),
                       arma_options = list(ar_order = NULL, ma_order = NULL),
                       bwidth_start = 0.15) {
  
  ts_name <- deparse(substitute(yt))
  
  nonpar_model <- deseats(yt, smoothing_options, bwidth_start)
  nonpar_model@ts_name <- ts_name
  
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
  
  out <- create_s_semiarma(decomp = decomp, nonpar_model = nonpar_model, 
                           par_model = par_est, ts_name = ts_name)
  out
}

#---------------

#'@export
methods::setMethod("predict", "s_semiarma", 
  function(object, n.ahead = 1, ...) {
    freq <- object@nonpar_model@frequency
    trend_e <- tail(trend(object), 2)
    
    times <- ceiling(n.ahead / freq)    
    season_e <- rep(tail(season(object), freq), times)
    
    trend_incr <- trend_e[[2]] - trend_e[[1]]
    
    trend_fc <- trend_e[[2]] + (1:n.ahead) * trend_incr
    
    season_fc <- season_e[1:n.ahead]
    
    arma_fc <- predict(object@par_model, n.ahead = n.ahead, se.fit = FALSE)
    
    trend_fc + season_fc + arma_fc
    
  }
)