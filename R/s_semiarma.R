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

#' Fitting of a Seasonal Semiparametric ARMA Model
#' 
#' Fit a seasonal semiparamteric autoregressive moving-average 
#' (S-Semi-ARMA) model to a univariate time series. The estimation is 
#' in two steps: firstly, the is detrended and seasonally adjusted using the
#' function \code{\link{deseats}}. Then an ARMA model is fitted to the 
#' residuals using \code{\link[stats]{arima}}. 
#'
#'@param yt a numerical vector or a time series object of class \code{ts} or 
#'that can be transformed with \code{\link[stats]{as.ts}} to an object of class 
#'\code{ts}; for these observations, trend and seasonality will be obtained.
#'@param smoothing_options an S4 object of class \code{smoothing_options}, which 
#'is returned by the function \code{\link{set_options}}; it 
#'includes details about the
#'options to consider in the locally weighted regression, such as the order of
#'polynomial and the bandwidth for smoothing among others, for the nonparametric
#'part of the model; the nonparametric model is fitted using 
#'\code{\link{deseats}}.
#'@param arma_options a list with the two elements \code{ar_order} and 
#'\code{ma_order} that indicates the AR and MA orders to consider for the 
#'parametric part of the model.
#'@param bwidth_start a single numeric value that is only relevant if the slot
#'\code{bwidth} in \code{smoothing_options} is set to \code{NA}; 
#'as the bandwidth will then
#'be selected automatically, \code{bwidth_start} sets the initial bandwidth for 
#'the algorithm.
#'
#'@export
#'
#'@details
#'
#'Work in progress. Details will be added in the release version of the package.
#'
#'@return
#'
#'The function returns and S4 object with the following elements (access them 
#'via \code{@}):
#'
#'@author
#'\itemize{
#'\item Dominik Schulz (Research Assistant) (Department of Economics, Paderborn
#'University), \cr
#'Author and Package Creator
#'\item Yuanhua Feng (Department of Economics, Paderborn
#'University), \cr
#'Author
#'}
#'
#'@examples
#'
#'Xt <- log(EXPENDITURES)
#'est <- s_semiarma(Xt)
#'est
#'
s_semiarma <- function(yt, smoothing_options = set_options(),
                       arma_options = list(ar_order = NULL, ma_order = NULL),
                       bwidth_start = 0.1) {
  
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

#'@export
methods::setMethod("show", "s_semiarma", 
  function(object) {
    text_out <- paste0(
    "\n*******************************************\n",
    "*                                         *\n",
    "*        Fitted Seasonal Semi-ARMA        *\n",
    "*                                         *\n",    
    "*******************************************\n\n",
    
    "Series: ", object@ts_name, "\n\n",
    
    "Nonparametric part (Trend + Seasonality):\n",
    "-----------------------------------------\n\n",
    "  Bandwidth: ", sprintf("%.4f", object@nonpar_model@bwidth), "\n\n",
    "  Trend:\n",
    "  ------\n",
    "    Order of local polynomial: ", object@nonpar_model@order_poly, "\n\n",
    "  Seasonality:\n",
    "  ------------\n",
    "    Frequency: ", object@nonpar_model@frequency, "\n\n",   
    "Parametric part (Rest):\n",
    "-----------------------\n",
    paste0(capture.output(print(object@par_model)), collapse = "\n"), "\n\n"
  )
  cat(text_out)
}
)