mu_select <- function(kernel_fun) {
  switch(kernel_fun,
    "uniform" = 0,
    "epanechnikov" = 1,
    "bisquare" = 2,
    "triweight" = 3
  )
}

bf_select <- function(boundary_method) {
  switch(boundary_method,
    "extend" = 1,
    "shorten" = 0
  )
}

#' Obtain frequency from given time series

define_frequency <- function(y, season_input) {

  if (is.na(season_input)) {
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
    s <- season_input
  }
  
  s
  
}

#' Obtain all smoothing values from input

determine_options <- function(y, smoothing_options) {
  
  list(
    bb = bf_select(smoothing_options@boundary_method),
    mu = mu_select(smoothing_options@kernel_fun),
    p = smoothing_options@order_poly,
    s = define_frequency(y, smoothing_options@season)
  )
  
}

#' Determine the inflation setting
determine_InfR <- function(inflation_rate, p) {
  
  if (all(inflation_rate == c("optimal", "naive"))) {
    inflation_rate <- c("optimal", NA, "naive")[[p]]
  }
  
  list("optimal" = c(5 / 7, NA, 9 / 11), "naive" = c(5 / 9, NA, 9 / 13))[[inflation_rate]][[p]]  
    
}

determine_err <- function(autocor) {
  
  as.numeric(autocor)
  
}

determine_CF <- function(correction_factor, err_m) {
  
  c(0, 1)[[(correction_factor + 1) * err_m + (err_m == 0)]]
  
}

determine_err_m <- function(error_model = c("free", "ARMA")) {
  
  error_model <- match.arg(error_model)
  c("free" = 1, "ARMA" = 0)[[error_model]]
  
}

determine_drop <- function(drop, p) {
  
  if (is.null(drop)) {
    drop <- c(0.05, NA, 0.1)[[p]]
  }
  
  drop
  
}

determine_bwidth_start <- function(bwidth_start, p) {
  
  if (is.null(bwidth_start)) {
    bwidth_start <- c(0.1, NA, 0.2)[[p]]
  }  
  
  bwidth_start
  
}

determine_alg <- function(
    inflation_rate, 
    autocor, 
    correction_factor, 
    error_model,
    drop,
    bwidth_start,
    p
  ) {
  
    err_m <- determine_err_m(error_model)
    
    list(
      infr = determine_InfR(inflation_rate, p),
      err = determine_err(autocor),
      CF = determine_CF(correction_factor, err_m),
      err_m = err_m,
      drop = determine_drop(drop, p),
      bwidth_start = determine_bwidth_start(bwidth_start, p)
    )
  
}


set_default <- function(obj, repl) {
  arg_names <- names(repl)
  for (i in seq_along(arg_names)) {
    if (is.null(obj[[arg_names[[i]]]])) {
      obj[[arg_names[[i]]]] <- repl[[i]]
    }
  }
  obj
}

maInfty <- function(ar, ma, m = 1000) {
  p <- length(ar)
  q <- length(ma)
  if (m - q > 0) {
    times <- m - q
  } else {
    times <- 0
  }
  ma.s <- c(ma, rep(0, times = times))
  c.out <- c(rep(0, times = p - 1), 1, rep(NA, times = m))
  lc <- length(c.out)

  if (m >= 1) {
    for (i in (p + 1):(m + p)) {
      c.out[i] <- ar %*% c.out[(i - 1):(i - p)] + ma.s[i - p]
    }
  }

  c.out[p:lc]
}


check_which <-function(which) {
  
    if (is.null(which)) {
    text_prompt <- data.frame(
      c("(1) facets:", "(2) observations:", "(3) fitted:", "(4) detailed_fit:", "(5) trend_season:",
        "(6) residuals:",
        "(7) deseasonalized:", "(8) detrended:"),
      c(
        "Facet plot of the estimated components",
        "The observed time series",
        "The observations together with estimated trend + seasonality",
        "The observations together with the fitted values and with the estimated trend",
        "The observations together with the estimated trend and the estimated seasonality separately",
        "The both detrended and seasonally adjusted series",
        "The seasonally adjusted series",
        "The detrended series"
      )
    )
    colnames(text_prompt) <- NULL
    
    cat("\nSelect one of the following plots via the keyword or the position number (exit with 0):\n")
    
    print.data.frame(text_prompt, row.names = FALSE, right = FALSE)
    
    cat("\n")
    
    which <- ""
    
    while(!(which %in% c(0, 1, 2, 3, 4, 5, 6, 7, 8, "facets", "observations", "fitted",
                         "detailed_fit", "trend_season", "residuals", "deseasonalized", 
                         "detrended"))) {
    
        which <- readline("Keyword or position number: ")
        
    }
    cat("\n")
    }
  
    which
}


check_which_hf <- function(which) {
  
    if (is.null(which)) {
    text_prompt <- data.frame(
      c("facets:", "observations:", "fitted:", "residuals:"),
      c(
        "Facet plot of the estimated components",
        "The observed time series",
        "The observations together with estimated trend + seasonality",
        "The both detrended and seasonally adjusted series"
      )
    )
    colnames(text_prompt) <- NULL
    
    cat("\nSelect one of the following plots via the keyword or the position number (exit with 0):\n")
    
    print.data.frame(text_prompt, row.names = FALSE, right = FALSE)
    
    cat("\n")
    
    which <- ""
    
    while(!(which %in% c(0, 1, 2, 3, 4, "facets", "observations", "fitted",
                         "residuals"))) {
    
        which <- readline("Keyword or position number: ")
        
    }
    cat("\n")
    }
  
    which
}

aniSave <- function(fun) {
  switch(
    fun,
    HTML = animation::saveHTML,
    Latex = animation::saveLatex,
    GIF = animation::saveGIF
  )
}

select_arma_orders <- function(xt, ar, ma) {
  
  if (is.null(ar) && is.null(ma)) {
    n <- length(xt)
    bic <- matrix(NA, nrow = 4, ncol = 4)
    P <- Q <- 0:3
    for (p0 in P) {
      for (q0 in Q) {
        arma <- tryCatch({
          suppressWarnings(stats::arima(xt,
            order = c(p0, 0, q0),
            include.mean = FALSE))
          }, error = function(e1) {data.frame(loglik = -10000000)}
        )
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
  c(ar, ma)
}