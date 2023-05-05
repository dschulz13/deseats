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
    calendar_data = NULL,
    tau = 3
) {
  
  ts_name <- deparse(substitute(yt))
  
  freq <- frequency(yt)
  
  calendar <- match.arg(calendar)
  
  if (freq == 4) {
    prev <- "quarterly"
  } else if (freq == 12) {
    prev <- "monthly"
  }
  
  df <- data.frame(
    Fx = residuals(BV4.1_base(yt, type = type))
  )
  
  if (outlier) {
    outl <- outlier_detect.bv41(yt, tau = tau, M = NULL)
    outl_signs <- outl$sign
    outl_signs <- outl_signs[outl_signs != 0]
    outlier_comp1 <- outl$outlier_dummy
    outlier_comp <- as.data.frame(lapply(
      as.data.frame(outlier_comp1),
      MARGIN = 2,
      FUN = function(x, type){residuals(BV4.1_base(x, type = type))},
      type = prev
    ))
    names(outlier_comp) <- paste0("outlier", seq_len(outlier_comp))
    df <- cbind(df, outlier_comp)
  } else {
    outlier_comp <- NULL
  }
  
  if (!is.null(shifts)) {
    
    shift_comp1 <- jump_dummy.bv41(yt, shifts)
    shift_comp <- as.data.frame(lapply(
      as.data.frame(shift_comp1),
      MARGIN = 2,
      FUN = function(x, type){residuals(BV4.1_base(x, type = type))},
      type = prev
    ))
    names(shift_comp) <- paste0("shift", seq_len(outlier_comp))    
    df <- cbind(df, shift_comp)
  } else {
    shift_comp <- NULL
  }
  
  if (!is.null(user)) {
    
    user_comp <- as.data.frame(lapply(
      user,
      MARGIN = 2,
      FUN = function(x, type){residuals(BV4.1_base(x, type = type))},
      type = prev
    ))
    names(user_comp) <- paste0("user", seq_len(outlier_comp))     
    df <- cbind(df, user_comp)
    
  } else {
    
    user_comp <- NULL
    
  }
  
  if (outlier | !is.null(shifts) | !is.null(user) | calendar != "None") {
     
     ols <- stats::lm(Fx ~ . - 1, data = df)
     outl_coef <- ols$coefficients[paste0("outlier", seq_len(outlier_comp))]
     outl_effects <- c(t(outl_coef) %*% outlier_comp1)
     check_signs <- all(sign(outl_effects) == sign(outl_signs)) #???
     
     
     
    
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
