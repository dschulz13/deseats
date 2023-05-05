#' Locally Weighted Regression for Trend and Seasonality in Equidistant Time 
#' Series under Short Memory
#' 
#' Simultaneously estimate the trend and the 
#' seasonality via locally weighted regression in an equidistant time series 
#' under short memory. The default setting uses an iterative plug-in algorithm 
#' for identifying the asymptotically globally optimal bandwidth for smoothing.
#'
#'@param y a numerical vector or a time series object of class \code{ts} or 
#'that can be transformed with \code{\link[stats]{as.ts}} to an object of class 
#'\code{ts}; for these observations, trend and seasonality will be obtained.
#'@param smoothing_options an S4 object of class \code{smoothing_options}, which 
#'is returned by the function \code{\link{set_options}}; it 
#'includes details about the
#'options to consider in the locally weighted regression such as the order of
#'polynomial and the bandwidth for smoothing among others.
#'@param bwidth_start a single numeric value that is only relevant if the slot
#'\code{bwidth} in \code{smoothing_options} is set to \code{NA}; 
#'as the bandwidth will then
#'be selected automatically, \code{bwidth_start} sets the initial bandwidth for 
#'the algorithm.
#'
#'@export
#'
#'@useDynLib deseats
#'
#'@details
#'
#'The trend is estimated based on the additive
#'nonparametric regression model for an equidistant time series
#'\deqn{y_t = m(x_t) + s(x_t) + \epsilon_t,}
#'where \eqn{y_t} is the observed time series, \eqn{x_t} is the rescaled time
#'on the interval \eqn{[0, 1]}, \eqn{m(x_t)} is a smooth and deterministic
#'trend function, \eqn{s(x_t)} is a (slowly changing) seasonal component with 
#'exact seasonal period \eqn{p_s} and \eqn{\epsilon_t} are stationary errors 
#'with \eqn{E(\epsilon_t) = 0} and short-range dependence (see for example also 
#'Feng, 2013, for a similar model, where the stochastic term is however i.i.d.). 
#'With this function \eqn{m(x_t)} and \eqn{s(x_t)} can be 
#'estimated without a parametric
#'model assumption for the error series. Thus, after estimating and removing
#'the trend and the seasonality, any suitable parametric model, e.g. an 
#'ARMA(\eqn{p,q}) model, can be fitted to the residuals 
#'(see \code{\link[stats]{arima}}).
#'
#'The iterative-plug-in (IPI) algorithm, which numerically minimizes the
#'Asymptotic Mean Squared Error (AMISE), was proposed by Feng and Schulz (2022) 
#'and is 
#'an extension of Feng (2013) to the short-memory case or of Feng, Gries and 
#'Fritz (2020) to a model with a seasonal component.
#'
#'Define \eqn{I[m^{(k)}] = \int_{c_b}^{d_b} [m^{(k)}(x)]^2 dx}{I[m^(k)] =
#'int_[c_b]^[d_b] \{m^(k)\}^2 dx}, \eqn{\beta_{(k)} = \int_{-1}^{1} u^k
#'K(u) du}{\beta_(k) = int_[-1]^[1] u^k K(u) du}
#'and \eqn{R(K) = \int_{-1}^{1} K^{2}(u) du}{R(K) = int_[-1]^[1]
#'\{K\}^2 du}, where \eqn{p} is the order of the (local) polynomial 
#'considered for \eqn{m},
#'\eqn{k = p + 1} is the order of the asymptotically equivalent kernel \eqn{K} 
#'for estimating \eqn{m}, \eqn{0 \leq c_{b}< d_{b} \leq 1}{0 \le c_b < d_b \le 1}, and
#'\eqn{c_f} is the variance factor, i.e. the sum of autocovariances divided by 
#'\eqn{2\pi}.
#'
#'Furthermore, we define
#'\deqn{C_{1} = \frac{I[m^{(k)}] \beta_{(k)}^2}{(k!)^2}}{C_1 =
#'[I[m^(k)]\{\beta_(k)\}^2] / (k!)^2}
#'and
#'\deqn{C_{2} = \frac{2 \pi c_{f} (d_b - c_b)[R(K) + (p_s - 1) R(W)]}{nh}}{C_2 =
#'2\pi(d_b - c_b)R(K)c_f / (nh)}
#'with \eqn{h} being the bandwidth, \eqn{n} being the number of
#'observations and \eqn{W} being the weighting function considered in the 
#'weighted least squares approach, for example a second-order kernel function 
#'with support on \eqn{[-1,1]}. The AMISE is then
#'\deqn{AMISE(h) = h^{2k}C_{1} + C_{2}.}{AMISE(h) = h^[2k] C_1 +
#'C_2.}
#'
#'The function calculates suitable estimates for \eqn{c_f}, the variance
#'factor, and \eqn{I[m^{(k)}]}{I[m^(k)]} over different iterations. In each
#'iteration, a bandwidth is obtained in accordance with the AMISE that once
#'more serves as an input for the following iteration. The process repeats
#'until either convergence or the 40th iteration is reached. For further
#'details on the asymptotic theory or the algorithm, please consult Feng,
#'Gries and Fritz (2020) or Feng et al. (2019).
#'
#'To apply the function, only few arguments are needed: a data input \code{y},
#'an object with smoothing options \code{smoothing_options} returned by 
#'\code{\link{set_options}} and 
#'a starting value for the relative bandwidth
#'\code{bwidth_start}. Aside from \code{y}, each argument has a default setting.
#'By default, a local linear trend is considered. In some cases, a local cubic 
#'trend may, however, be more suitable. For more specific information on the input arguments
#'consult the section \emph{Arguments}.
#'
#'When applying the function, an optimal bandwidth is obtained based on the
#'IPI algorithm proposed by Feng, Gries and Fritz (2020). In a second step,
#'the nonparametric trend of the series and the seasonality are calculated with 
#'respect to the chosen bandwidth.
#'
#'The default print method for this function delivers key numbers such as the 
#'bandwidth considered for smoothing.
#'
#'NOTE:
#'
#'This function implements C++ code by means
#'of the \code{\link[Rcpp:Rcpp-package]{Rcpp}} and
#'\code{\link[RcppArmadillo:RcppArmadillo-package]{RcppArmadillo}} packages for
#'better performance.
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
#'est <- deseats(Xt)
#'est
#'plot(est, which = 1)
#'

deseats <- function(y, smoothing_options = set_options(), bwidth_start = 0.1) {

  ts_name <- deparse(substitute(y))
  
  y <- stats::as.ts(y)
  att <- attributes(y)
  
  if (is.na(smoothing_options@season)) {
    if (inherits(y, c("ts", "zoo", "xts"))) {
      s <- stats::frequency(y)
      if (s == 1) {
        stop('A seasonal period is required in the element "@season" of ', 
             'the argument "smoothing_options".')
      }
    } else {
      s <- 1
    }
  } else {
    s <- smoothing_options@season
  }
  
  # Number of observations
  n <- length(y)           

  # Select numeric values from character input using helper functions
  bf <- bf_select(smoothing_options@boundary_method)
  mu <- mu_select(smoothing_options@kernel_fun)  
  p <- smoothing_options@order_poly
  bwidth <- smoothing_options@bwidth
  if (is.na(bwidth)) {
    bwidth <- algorithmCpp(y, p, s, mu, bwidth_start)
  }

  est <- fitteddeseatsCpp(y, p, s, mu, bwidth, bf)
  Trend <- est[[2]]
  Season <- est[[3]]
  Residuals <- est[[4]]
  sum_autocov <- cf0Cpp(Residuals)
  hfull <- length(est[[5]][, 1])
  weights <- array(c(est[[5]], est[[6]], est[[7]]), dim = c(hfull, hfull, 3))
  dimnames(weights)[[3]] <- c("Trend", "Season", "Combined")
  attributes(Trend) <- att
  attributes(Season) <- att
  attributes(Residuals) <- att
  decomp <- cbind(
    Observations = y,
    Trend = Trend,
    Season = Season,
    Rest = Residuals
  )
  
  out <- create_deseats(
    decomp = decomp,
    bwidth = bwidth,
    ts_name = ts_name,
    frequency = s,
    order_poly = p, 
    weights = weights,
    weights_wfun = est[[8]],
    boundary_method = smoothing_options@boundary_method,
    kernel_fun = smoothing_options@kernel_fun,
    sum_autocov = sum_autocov
  )
  
  out
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
  
  ar_ma_select <- select_arma_orders(res, arma_options$ar_order, 
                                     arma_options$ma_order)  
  
  ar <- ar_ma_select[[1]]
  ma <- ar_ma_select[[2]]

  par_est <- stats::arima(res, order = c(ar, 0, ma),
                        include.mean = FALSE)

  par_est <- stats::arima(res, order = c(ar, 0, ma),
                        include.mean = FALSE)

  arma_res <- par_est$residuals
  attributes(arma_res) <- attributes(nonpar_model@decomp[, 1])
  arma_fitted <- residuals(nonpar_model) - arma_res
  
  decomp <- cbind(nonpar_model@decomp, arma_fitted, arma_res)
  colnames(decomp) <- c(
    "Observations",
    "Trend",
    "Season",
    "Rest",
    "Fitted ARMA",
    "Residuals ARMA"
  )
  
  out <- create_s_semiarma(decomp = decomp, nonpar_model = nonpar_model, 
                           par_model = par_est)
  out
}

#------------------------------------------------------------------------------#

#'Trend and Seasonality Estimation Using the Berlin Procedure 4.1
#'
#'Economic data can be decomposed into a trend, a seasonal and a remainder 
#'component using the Berlin procedure  4.1 (German: Berliner Verfahren 
#'4.1), as used by the National Statistical Office of Germany. All further 
#'component estimations, for example the estimation of the
#'calendar component, of the official procedure are not yet implemented. The 
#'function supports quarterly and monthly data. 
#'
#'@param yt a time series object of class \code{ts} or an object that can be 
#'converted into such an object with \code{\link[stats]{as.ts}}.
#'@param type a single character value that indicates, whether the data was
#'quarterly (\code{"quarterly"}) or monthly (\code{"monthly"}) observed; the 
#'default is \code{"monthly"}; if a time series object is passed to \code{yt},
#'the value for this argument will be automatically selected according to the 
#'frequency in \code{yt}. 
#'
#'@details
#'
#'Work in progress. Details will soon be added.
#'
#'The advantage of the Berlin Procedure 4.1 (BV4.1) is that it makes use of
#'fixed filters based on locally weighted regression (both with a weighted 
#'mixture of local linear and local cubic components for the trend) at all 
#'observation time points. Thus, BV4.1 results in fixed weighting matrices 
#'both for the trend estimation step and for the seasonality estimation step 
#'that can be immediately applied to all economic time series. 
#'Those matrices are saved internally in the package and when applying BV4.1, 
#'only weighted sums of the observations (with already obtained weights) have 
#'to be obtained at all time points. Thus, this procedure is quite fast.
#'
#'@export
#'
#'@references
#'Speth, H.-T. (2004). Komponentenzerlegung und Saisonbereinigung ökonomischer 
#'Zeitreihen mit dem Verfahren BV4.1. Statistisches Bundesamt.
#'
#'@author
#'\itemize{
#'\item Dominik Schulz (Research Assistant) (Department of Economics, Paderborn
#'University), \cr
#'Author and Package Creator
#'}
#'
#'@examples
#'
#'Xt <- LABORFORCE
#'est <- BV4.1(Xt)
#'est
#'
BV4.1_base <- function(yt, type = NULL) {
  
  ts_name <- deparse(substitute(yt))
  
  if (!is.na(type) && length(type) == 2 && all(type) == c("monthly", "quarterly")) {
    type = "monthly"
  }
  
  yt <- stats::as.ts(yt)
  att <- attributes(yt)
  freq <- stats::frequency(yt)
  if (is.null(type)) {
    type <- switch(
      as.character(freq),
      "4" = "quarterly",
      "12" = "monthly",
      stop(paste0("The procedure cannot be applied to data with frequency ", freq, "."))
    )
  }
  
  max_vals <- switch(
    type,
    monthly = 30,
    quarterly = 11
  )
  max_vals2 <- switch(
    type,
    monthly = 51,
    quarterly = 17
  )  
  n_rows <- switch(
    type,
    monthly = 13,
    quarterly = 4
  )
  n_rows2 <- switch(
    type,
    monthly = 23,
    quarterly = 7
  )
  
  trend_mat <- bv_mats[[type]][["trend"]]
  season_mat <- bv_mats[[type]][["season"]]
  
  results <- BV41filterCpp(
    xt = c(yt),
    wst = trend_mat,
    wss = season_mat,
    nrowst = n_rows,
    nrowss = n_rows2,
    maxt = max_vals,
    maxs = max_vals2
  )
  
  trend_e <- results$trend_e
  season_e <- results$season_e
  
  attributes(trend_e) <- att
  attributes(season_e) <- att  
  
  rest <- yt - trend_e - season_e
  
  decomp <- cbind(Observations = yt, Trend = trend_e, Season = season_e, Rest = rest)
  out <- create_bv41(decomp, ts_name, freq)
  out
  
}

#'@export
BV4.1_arma <- function(yt, 
                  arma_options = list(ar_order = NULL, ma_order = NULL)) {
  
  ts_name <- deparse(substitute(yt))
  
  nonpar_model <- BV4.1(yt)
  nonpar_model@ts_name <- ts_name
  
  res <- residuals(nonpar_model)
  
  ar_ma_select <- select_arma_orders(res, arma_options$ar_order, 
                                     arma_options$ma_order)
  
  ar <- ar_ma_select[[1]]
  ma <- ar_ma_select[[2]]

  par_est <- stats::arima(res, order = c(ar, 0, ma),
                        include.mean = FALSE)

  arma_res <- par_est$residuals
  attributes(arma_res) <- attributes(nonpar_model@decomp[, 1])
  arma_fitted <- residuals(nonpar_model) - arma_res
  
  decomp <- cbind(nonpar_model@decomp, arma_fitted, arma_res)
  colnames(decomp) <- c(
    "Observations",
    "Trend",
    "Season",
    "Rest",
    "Fitted ARMA",
    "Residuals ARMA"
  )
  
  out <- create_bv41_arma(decomp = decomp, nonpar_model = nonpar_model, 
                           par_model = par_est)
  out
}


#------------------------------------------------------------------------------#

#'Time Series Filtering Using the Hamilton Filter
#'
#'A stationary remainder is obtained from a univariate time series using the 
#'filter proposed by Hamilton. The filter is capable of estimating the trend 
#'together with the seasonality in a series.
#'
#'@param yt a time series object of class \code{ts} or an object that can be 
#'transformed to that class using \code{\link[stats]{as.ts}}.
#'@param h the backwards time skip for the first regressor; the default is 
#'the seasonal period in \code{yt} multiplied by 2.
#'@param p the number of regressors; the default is the seasonal period in
#'\code{yt}.
#'
#'@details
#'Soon to be written.
#'
#'@export
#'
#'@examples
#'
#'est <- hamilton_filter(log(EXPENDITURES))
#'est
#'

hamilton_filter <- function(yt, h = NULL, p = NULL) {

  ts_name <- deparse(substitute(yt))
  
  s <- stats::frequency(yt)
  h <- 2 * s
  p <- s
  
  n <- length(yt)
  
  t <- c(stats::time(yt))
  t_start <- utils::tail(t, n - (h + p - 1))[[1]]
  
  # The dependent variable for OLS
  X.dep <- utils::tail(yt, n - h - (p - 1))
  
  # Create a matrix with all independent variables
  X.ind <- matrix(NA, ncol = p, nrow = length(X.dep))
  for (i in 1:p) {
    X.ind[, i] <- yt[(p - (i - 1)):(n - h - (i - 1))]
  }
  
  # Combine the dependent and independent variables and create 
  # a dataframe with specific column names
  dataset <- as.data.frame(cbind(X.dep, X.ind))
  colnames(dataset) <- c("X.dep", paste0("X.ind", 1:p))
  
  # Conduct OLS and retrieve the coefficients, the fitted values and
  # the residuals
  est <- stats::lm(X.dep ~ ., data = dataset)
  coefs <- est[["coefficients"]]

  fit <- stats::ts(est[["fitted.values"]], start = t_start, frequency = s)
  res <- stats::ts(est[["residuals"]], start = t_start, frequency = s)
  
  # Create and return the output
  out <- create_hfilter(
    decomp = cbind(Observations = yt, Fitted = fit, Rest = res),
    ts_name = ts_name,
    frequency = s,
    regression_output = est
  )
  out  
  
}

#------------------------------------------------------------------------------#

#'Decomposition of Time Series Using Linear Regression
#'
#'Trend and seasonality are simultaneously modelled by considering a polynomial
#'for the trend and dummy variables for the different time units (e.g. months).
#'
#'@param yt a time series object of class \code{ts} or an object that can be 
#'transformed to that class using \code{\link[stats]{as.ts}}.
#'@param order_poly the order of the polynomial considered for the trend; the
#'default is \code{order_poly = 1}.
#'@param season the seasonal period in \code{yt}; by default, the seasonal 
#'period is obtained automatically from \code{yt}.
#'
#'@details
#'Soon to be written.
#'
#'@export
#'
#'@examples
#'
#'est <- lm_decomp(log(EXPENDITURES), order_poly = 4)
#'est
#'

lm_decomp <- function(yt, order_poly = 1, season = NULL) {

  ts_name <- deparse(substitute(yt))
  if (is.null(season)) {
    season <- frequency(yt)
  }
  
  N <- length(yt)
  season_add_col <- if (season > 1) {season - 1} else {0}
  X_mat <- matrix(0, nrow = N, ncol = order_poly + season_add_col)
  if (season > 1) {
    diag_N <- floor(N / season)
    rest_rows <- N - diag_N * season
    for (i in 1:diag_N) {
      X_mat[(1:season) + ((i - 1) * season), 
            (1 + order_poly):(order_poly + season - 1)] <- diag(nrow = season, 
                                                              ncol = season - 1)
    }
    if (rest_rows >= 1) {
      X_mat[(N - rest_rows + 1):N, 
          (1 + order_poly):(order_poly + season - 1)] <- diag(nrow = rest_rows, 
                                                              ncol = season - 1)
    }
    X_mat[(1:N) %% season == 0, ] <- rep(-1, order_poly + season - 1)
    name_season <- paste0("S", 1:(season - 1))
  } else {
    name_season <- c()
  }
  if (order_poly > 0) {
    t <- seq_along(yt)
    for (i in 1:order_poly) {
      X_mat[, i] <- t^i
    }
    name_trend <- paste0("t", 1:order_poly)
  } else {
    name_trend <- c()
  }
  X_df <- as.data.frame(cbind(
    as.numeric(yt),
    X_mat
    ))
  names(X_df) <- c("yt", name_trend, name_season)
  reg <- stats::lm(yt ~ ., data = X_df)

  # if (season > 1) {
    s <- reg$coefficients[(2 + order_poly):(order_poly + season)]
    seasonality <- utils::head(rep(c(s, -sum(s)), diag_N + 1), N)
    remainder <- yt - seasonality
    name_s <- "Season"
  # } else {
  #   seasonality <- c()
  #   name_s <- seasonality
  #   remainder <- yt
  # }
  # if (order_poly > 1) {
    # if (season > 1) {
      X_df[, (2 + order_poly):(1 + order_poly + season_add_col)] <- 0
    # }
    trend <- unname(predict(reg, X_df))
    remainder <- remainder - trend
    name_t <- "Trend"
  # } else {
  #   trend <- c()
  #   name_t <- trend
  # }
  
  out <- create_lmdecomp(
    decomp = cbind("Observations" = yt, "Trend" = trend, 
          "Season" = seasonality, "Rest" = remainder),
    ts_name = ts_name,
    frequency = season,
    regression_output = reg
  )

  out
}