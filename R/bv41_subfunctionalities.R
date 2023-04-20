#' Autocovariance Matrix

acov_matrix <- function(xt, M) {
  
  acov <- c(stats::acf(xt, lag.max = M, type = "covariance")$acf)
  
  Gamma_Mp1 <- matrix(0, nrow = M + 1, ncol = M + 1)
  Gamma_Mp1[lower.tri(Gamma_Mp1, diag = TRUE)] <- acov
  Gamma_Mp1 <- t(Gamma_Mp1)
  Gamma_Mp1[lower.tri(Gamma_Mp1, diag = TRUE)] <- acov
  
  Gamma_Mp1
  
}

#' BV4.1 Outlier Component

outlier_comp.bv41 <- function(xt, tau = 3, M = NULL) {
  
  freq <- frequency(xt)
  stopifnot(freq %in% c(4, 12))
  
  xt <- c(xt)
  n <- length(xt)
  
  if (is.null(M)) {
  
    if (freq == 4) {
    
      M <- 8
    
    } else if (freq == 12) {
    
      M <- 24
    
    }
    
  }
  
  # Autocovariance matrix
  
  Gamma_Mp1 <- acov_matrix(xt = xt, M = M)
  
  # Mean
  
  v <- mean(xt)
  
  # Conditional means
  
  E_xi <- rep(NA, n)
  
  for (i in (M + 1):n) {
    
    E_xi <- v - (1 / Gamma_Mp1[1, 1]) * Gamma_Mp1[1, 2:(M + 1), drop = FALSE] %*% (xt[(i - 1):(i - M)] - v)
    
  }

}

