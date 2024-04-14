diff3_var <- function(x) {
  n <- length(x)
  sum(diff(x, lag = 3)^2) / (n - 3)
}

mean_diff <- function(x, k) {
  sum(diff(x, lag = k)) / (n - k)
}

M1 <- function(trend, season, irregular) {
  I2 <- diff3_var(irregular)
  T2 <- diff3_var(trend)
  S2 <- diff3_var(season)
  
  out <- I2 / (I2 + T2 + S2) * 10
  if (out < 1) {out <- 1}
  if (out > 3) {out <- 3}
  out
}

M2 <- function(trend, season, irregluar) {
  
}

M3 <- function(trend, irregular, frequency) {
  T_bar <- mean(abs(diff(trend)))
  I_bar <- mean(abs(diff(irregular)))
  out <- (I_bar / T_bar - (frequency / 12)) / (frequency / 6)
  if (out < 1) {out <- 1}
  if (out > 3) {out <- 3}
  out  
}

M4 <- function(irregular) {
  n <- length(irregular)  
  #ADR <- 3 * (n + 7 - 4 * exp(1)) / (2 * n - 7)
  out <- (abs((n - 1) / ADR - 2 * (n - 1) / 3) / ((16 * n - 29) / 90)^0.5) / 2.58
  if (out < 1) {out <- 1}
  if (out > 3) {out <- 3}
  out  
}

M5 <- function(trend, irregular) {
  f <- frequency(irregular)
  continuer <- TRUE
  k <- 0
  ratio_old <- 0
  ratio_new <- 0
  while (continuer) {
    k <- k + 1
    ratio_old <- ratio_new
    I_bar <- mean_diff(irregular, k = k)
    T_bar <- mean_diff(trend, k = k)
    ratio_new <- I_bar / T_bar
    if (k >= 2 & ratio_new <= 1 & ratio_old > 1) {
      continuer <- FALSE
    }
  }
  
  MCD_a <- 2 + (ratio_old - 1) / (ratio_old - ratio_new)
  
  out <- (MCD_a - f / 24) / (f * 5 / 12)
  
  if (out < 1) {out <- 1}
  if (out > 3) {out <- 3}
  out    
}

M6 <- function(seasonal, irregular) {
  out <- abs((mean_diff(irregular, k = 0) / mean_diff(seasonal, k = 0) - 4) / 2.5)
  if (out < 1) {out <- 1}
  if (out > 3) {out <- 3}
  out      
}

M7 <- function(trend, irregular) {
  
}

M8 <- function(trend, irregular) {
  
}

M9 <- function(trend, irregular) {
  
}

M10 <- function(trend, irregular) {
  
}

M11 <- function(trend, irregular) {
  
}