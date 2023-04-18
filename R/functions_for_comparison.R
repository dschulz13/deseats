#'Estimation of Time Series Trends Via a Global Polynomial
#'
#'@export
#'

trend_poly <- function(yt, p = 1) {
  zt <- c(yt)
  P <- frequency(yt)
  n <- length(yt)
  t <- (1:n) / n
  form <- as.formula(
    paste0("zt ~ ", paste0("t^", 1:p, collapse = " + "))
  )
  est <- lm(formula = form)
  est
}

#'Estimation of Time Series Seasonality Via a Global Trigonometric Polynomial
#'
#'@export
#'

season_trigo <- function(yt) {
  P <- frequency(yt)
  n <- length(yt)
  t <- 1:n
  trigo_vars <- vector(mode = "list", length = P - 1)
  names(trigo_vars) <- paste0("c", 1:(P-1))
  lambda1 <- 2 * pi / P
  for (i in 1:(trunc((P - 1) / 2))) {
    trigo_vars[[2 * i - 1]] <- cos(i * lambda1 * t)
    trigo_vars[[2 * i]] <- sin(i * lambda1 * t)
  }
  if (P %% 2 == 0) {
    trigo_vars[[P - 1]] <- cos(pi * t)
  }
  trigo_vars[["yt"]] <- c(yt)
  df <- as.data.frame(trigo_vars)
  est <- lm(yt ~ . - 1, data = df)
  est
}


#'Estimation of Time Series Seasonality Via Dummy Variable Regression
#'
#'@export
#'

season_dummy <- function(yt) {
  P <- frequency(yt)
  yt <- c(yt)
  n <- length(yt)

  
  est <- lm(yt ~ period)
  est
}

#'Purely Parametric Time Series Decomposition
#'
#'@export
#'
#'@examples
#'xt <- CIVLABOR
#'est1 <- para_decomp(xt, p = 3, season = "trigo")
#'summary(est1)
#'
#'est2 <- para_decomp(xt, p = 3, season = "dummy")
#'summary(est2)
#'

para_decomp <- function(yt, p = 1, season = c("trigo", "dummy")) {
  season <- match.arg(season)
  
  P <- frequency(yt)
  n <- length(yt)
  t <- 1:n
  xt <- t / n
  
  if (season == "trigo") {
    vars <- vector(mode = "list", length = P - 1 + p)
    lambda1 <- 2 * pi / P
    for (i in 1:(trunc((P - 1) / 2))) {
      vars[[p + 2 * i - 1]] <- cos(i * lambda1 * t)
      vars[[p + 2 * i]] <- sin(i * lambda1 * t)
    }
    if (P %% 2 == 0) {
      vars[[p + P - 1]] <- cos(pi * t)
    }

    names(vars) <- c(paste0("xt^", 1:p), paste0("c", 1:(P-1)))  

  } else if (season == "dummy") {
    
    vars <- vector(mode = "list", length = 1 + p)    
    
    vars[[p + 1]] <- rep(paste0("d", 1:P), length.out = n)   
    
    names(vars) <- c(paste0("xt^", 1:p), "d")    
    
  }
  
  for (i in 1:p) {
    vars[[i]] <- xt^i
  }  
  vars[["yt"]] <- c(yt) 
  df <- as.data.frame(vars) 
  est <- lm(yt ~ ., data = df)
  
  est
}