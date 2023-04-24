#' Autocovariance Matrix

acov_matrix <- function(xt, M) {
  
  acov <- c(stats::acf(xt, lag.max = M, type = "covariance", plot = FALSE)$acf)
  
  Gamma_Mp1 <- matrix(0, nrow = M + 1, ncol = M + 1)
  Gamma_Mp1[lower.tri(Gamma_Mp1, diag = TRUE)] <- acov
  Gamma_Mp1 <- t(Gamma_Mp1)
  Gamma_Mp1[lower.tri(Gamma_Mp1, diag = TRUE)] <- acov
  
  Gamma_Mp1
  
}

kappa_M <- function(mat) {
  
  solve(mat)[1, , drop = TRUE]
  
}

calendar_CondValues <- function(xt, M, v, kappa) {
  
  n <- length(xt)
  
  E_xi <- rep(NA, n)
  
  for (i in (M + 1):n) {
    
    E_xi[[i]] <- v - (1 / kappa[[1]]) * (t(kappa[2:(M + 1)]) %*% (xt[(i - 1):(i - M)] - v))
    
  }  
  
  var_xi <- (1 / (n - M)) * sum((xt - E_xi)^2, na.rm = TRUE)
  
  list(cond_mean = E_xi, cond_var = var_xi)
  
}

#' BV4.1 Outlier Component

outlier_detect.bv41 <- function(xt, tau = 3, M = NULL, forward = TRUE) {
  
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
  
  kappa <- kappa_M(
    acov_matrix(xt, M = M)
  )
  v <- mean(xt)
  
  # Conditional means
  
  cond_values <- calendar_CondValues(xt, M = M, v = v, kappa = kappa)
  cond_mean <- cond_values$cond_mean
  cond_sd <- sqrt(cond_values$cond_var)
  
  outlier <- rep(FALSE, n)
  
  for (i in (M + 1):n) {
    
    outlier[[i]] <- xt[[i]] < cond_mean[[i]] - tau * cond_sd ||
      xt[[i]] > cond_mean[[i]] + tau * cond_sd
    
    if (outlier[[i]]) {
      if (xt[[i]] < cond_mean[[i]] - tau * cond_sd) {
        xt[[i]] <- cond_mean[[i]] - tau * cond_sd
      } else if (xt[[i]] > cond_mean[[i]] + tau * cond_sd) {
        xt[[i]] <- cond_mean[[i]] + tau * cond_sd      
      }
 
      cond_values <- calendar_CondValues(xt, M = M, v = v, kappa = kappa)
      cond_mean <- cond_values$cond_mean
      cond_sd <- sqrt(cond_values$cond_var)
      
    }
    
  }
  
  if (!forward) {
    yt <- rev(xt)
    attr(yt) <- attr(xt)
    outlier_backwards <- rev(outlier_detect.bv41(yt, forward = FALSE))
    
    outlier <- outlier | outlier_backwards
  }

  outlier
  
}

outlier_dummy.bv41 <- function(outlier) {
  
  n <- length(outlier)
  m <- sum(outlier)
  pos <- which(outlier)
  
  out_mat <- matrix(0, nrow = n, ncol = m) 
  
  for (i in 1:m) {
    
    out_mat[pos[[i]], i] <- 1 
    
  }
  
  colnames(out_mat) <- paste0("Outlier", 1:m)
  
  out_mat
  
}

### Jumping points

jump_dummy.bv41 <- function(jump_point, n) {
  
  s <- length(jump_point)
  mat <- matrix(0, ncol = s, nrow = n)
  
  for (i in 1:s) {
    
    mat[jump_point[[i]]:n, i] <- 1
    
  }
  
  mat
  
}
