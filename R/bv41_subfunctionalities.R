# BV4.1 Outlier Component

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

###############################

create_outlier_comp <- function(yt, tau, outlier = FALSE) {
  
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
  
  list(dummies = outlier_comp, signs = outl_signs)
  
}

##############################

create_shift_comp <- function(yt, shifts = NULL) {
  
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
  
  shift_comp
  
}

##############################

create_user_comp <- function(yt, shifts = NULL) {
  
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
  
}

##############################

bv41_ols <- function(df, outlier_obj) {
  
  ols <- stats::lm(Fx ~ . - 1, data = df)
  outlier_comp <- outlier_obj$outlier_dummy
  outlier_signs <- outlier_obj$signs
  outl_coef <- ols$coefficients[paste0("outlier", 1:ncol(outlier_comp))]
  outl_effects <- c(t(outl_coef) %*% as.matrix(outlier_comp))
  check_signs <- all(sign(outl_effects) == sign(outlier_signs))
  
  if (!check_signs) {
    
    which_sign_diff <- which(sign(outl_effects) != sign(outlier_signs))
    
    df[paste0("outlier", which_sign_diff)] <- NULL
    
    outlier_obj$outlier_dummy[paste0("outlier", which_sign_diff)] <- NULL
    rsums <- as.logical(rowSums(outlier_obj$outlier_dummy))
    outlier_obj$signs[!rsums] <- 0
    
    ols <- bv41_ols(df = df, outlier_comp = outlier_obj)
    
  }
  
  list(ols = ols, outlier_comp = outlier_comp)
  
}

##############################

#'Full BV4.1 Method
#'
#'@export
#'

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
  
  outlier_info <- create_outlier_comp(yt, tau = tau)
  outlier_comp <- outlier_info$dummies
  outlier_signs <- outlier_info$signs
  
  shift_comp <- create_shift_comp(yt, shifts = shifts)
  
  user_comp <- create_user_comp(user)
  
  df <- cbind(
    Fx = residuals(BV4.1_base(yt, type = type)),
    outlier = outlier_comp,
    shift = shift_comp,
    user = user_comp
  )
  
  if (outlier) {
    n_o <- length(outlier_comp[1, ])
    F_outlier_comp <- outlier_comp
    
    for (i in 1:n_o) {
      F_outlier_comp[, i] <- residuals(BV4.1_base(outlier_comp[, i], type = type))
    }
    
    df <- cbind(df, F_outlier_comp)
  }
  if (!is.null(shifts)) {
    
    n_s <- length(shift_comp[1, ])
    
    F_shift_comp <- shift_comp
    
    for (i in 1:n_s) {
      F_shift_comp[, i] <- residuals(BV4.1_base(shift_comp[, i], type = type))
    }    
    
    df <- cbind(df, F_shift_comp)
  }
  if (!is.null(user)) {
    
    n_u <- length(user_comp[1, ])
    
    F_user_comp <- user_comp
    
    for (i in 1:n_u) {
      F_user_comp[, i] <- residuals(BV4.1_base(user_comp[, i], type = type))
    }      
    
    df <- cbind(df, F_user_comp)
  }
  
  if (outlier | !is.null(shifts) | !is.null(user) | calendar != "None") {
     
    ols <- bv41_ols(df = df, outlier_comp = outlier_info)
    
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
