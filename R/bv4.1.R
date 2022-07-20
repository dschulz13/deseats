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
#'converted into such an object with \code{\link[stats]{as.ts()}}.
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
#'@export
#'
#'@references
#'Speth, H.-T. (2004). Komponentenzerlegung und Saisonbereinigung ökonomischer 
#'Zeitreihen mit dem Verfahren BV4.1. Statistisches Bundesamt.
#'
#'@examples
#'
#'Xt <- LABORFORCE
#'est <- BV4.1(Xt)
#'est
#'
BV4.1 <- function(yt, type = NULL) {
  
  
  
  if (!is.na(type) && length(type) == 2 && all(type) == c("monthly", "quarterly")) {
    type = "monthly"
  }
  
  yt <- as.ts(yt)
  att <- attributes(yt)
  freq <- frequency(yt)
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
  
  n <- length(yt)
  trend_e <- rep(NA, n)
  season_e <- trend_e
  
  N <- 2 * n_rows + 1
  N2 <- 2 * n_rows2 + 1
  
  trend_e[1:n_rows] <- trend_mat[1:n_rows, ] %*% head(yt, max_vals)
  ymd <- matrix(0, ncol = n - 2 * n_rows, nrow = N)
  ymd2 <- matrix(0, ncol = n - 2 * n_rows2, nrow = N2)
  for (i in (n_rows + 1):(n - n_rows)) {
    ymd[, i - n_rows] <- yt[(i - n_rows):(i + n_rows)]
  }
  trend_e[(n_rows + 1):(n - n_rows)] <- t(trend_mat[(n_rows + 1), 1:N]) %*% ymd
  trend_e[(n - (n_rows - 1)):n] <- trend_mat[(n_rows + 2):N, ] %*% tail(yt, max_vals)
  
  detrend <- yt - trend_e
  
  for (i in (n_rows2 + 1):(n - n_rows2)) {
    ymd2[, i - n_rows2] <- detrend[(i - n_rows2):(i + n_rows2)]
  } 
  
  season_e[1:n_rows2] <- season_mat[1:n_rows2, ] %*% head(detrend, max_vals2)  
  season_e[(n_rows2 + 1):(n - n_rows2)] <- t(season_mat[(n_rows2 + 1), 1:N2]) %*% ymd2
  season_e[(n - (n_rows2 - 1)):n] <- season_mat[(n_rows2 + 2):N2, ] %*% tail(detrend, max_vals2)
  
  attributes(trend_e) <- att
  attributes(season_e) <- att  
  
  rest <- yt - trend_e - season_e
  
  cbind(Observations = yt, Trend = trend_e, Season = season_e, Rest = rest)
  
}
