#ARIMA Function Wrapper Without Warnings
#
#For internal use only.

arima_no_warn <- function(xt, order = c(0L, 0L, 0L)) {
  suppressWarnings(stats::arima(xt, order = order))
}

farima_no_warn <- function(xt, p = 0, q = 0) {
  suppressWarnings(fracdiff::fracdiff(xt, nar = p, nma = q))
}

BIC_farima_no_warn <- function(xt, p = 0, q = 0) {
  est <- suppressWarnings(fracdiff::fracdiff(xt, nar = p, nma = q))
  k <- log(length(xt))
  suppressWarnings(AIC(est, k = k))
}
