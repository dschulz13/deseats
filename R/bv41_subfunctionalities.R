#' BV4.1 Outlier Component

outlier_detect.bv41 <- function(xt, tau = 3, M = NULL) {
  
  freq <- frequency(xt)
  stopifnot(freq %in% c(4, 12))
  
  xt <- c(xt)
  
  if (is.null(M)) {
  
    if (freq == 4) {
    
      M <- 8
    
    } else if (freq == 12) {
    
      M <- 24
    
    }
    
  }  
  
  # 1 indicates that the method should be applied forward in time
  
  # 1 also automatically calls 0 (= backward in time) and combines
  # the results
  outlierDetectBV41Cpp(xt, tau, M, 1)
  
}

### Jumping points

jump_dummy.bv41 <- function(xt, jump_points) {
  
  n <- length(xt)
  
  JumpVarsBV41Cpp(jump_points, n)
  
}

BV4.1 <- function(
    yt, 
    outlier = FALSE, 
    calendar = c("none", "weekdays", "workdaysMonFri", "workdaysMonSat"),
    shifts = NULL,
    user = NULL,
    calendar_data = NULL
) {
  
  ts_name <- deparse(substitute(yt))
  
  if (outlier) {
    outlier_comp <- outlier_detect.bv41(yt, tau = 3, M = NULL)
  } else {
    outlier_comp <- NULL
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
