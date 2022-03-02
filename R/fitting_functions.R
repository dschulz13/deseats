mu_select <- function(kernel_fun) {
  switch(kernel_fun,
    "Uniform" = 0,
    "Epanechnikov" = 1,
    "Bisquare" = 2,
    "Triweight" = 3
  )
}

bf_select <- function(boundary_method) {
  switch(boundary_method,
    "extend" = 1,
    "shorten" = 0
  )
}

weights_select <- function(mu) {
  switch(as.character(mu),
  "0" = function(lowB, upB, allsequ2, hr2) {rep(1, upB - lowB + 1)},
  "1" = function(lowB, upB, allsequ2, hr2) {1.0 - allsequ2[lowB:upB] / hr2},
  function(lowB, upB, allsequ2, hr2) {(1.0 - allsequ2[lowB:upB] / hr2)^mu}
  )
}

#------------------------------------------------------------------------------#

smooth_deseats <- function(y, period = 12, order_poly = 1, 
  kernel_fun = c("Uniform", "Epanechnikov", "Bisquare", "Triweight"), 
  bwidth = 0.15, boundary_method = c("extend", "shorten")) {

  # Number of observations
  n <- length(y)           

  # Select numeric values from character input using helper functions
  bf <- bf_select(boundary_method)
  mu <- mu_select(kernel_fun)  
  
  # Empty vectors for the trend and season estimates
  G <- rep(0, n)
  S <- rep(0, n)
  
  # Absolute bandwidth obtained from the relative bandwidth
  bwidth_absolute <- trunc(n * bwidth + 0.5)
  # Maximum (total) bandwidth to right side
  bwidth_max <- 2 * bwidth_absolute + 1      

  # Data matrix for the trend
  dataMat_G <- matrix(0, order_poly + period, bwidth_max) 
  # Data matrix for the season
  dataMat_S <- dataMat_G                 
  # Weighted data matrix
  dataMat_weighted <- dataMat_G                 

  seq_bwidth_abs <- 0:bwidth_absolute
  right_bwidth_extension <- bwidth_absolute - seq_bwidth_abs
  bwidth_right <- bwidth_absolute + bf * right_bwidth_extension
  bwidth_right_squared <- (bwidth_right + 1)^2  
  bwidth_total <- seq_bwidth_abs + bwidth_right

  # All (centered) t-values to consider for all time points (only a subset is
  # considered for each observation time point)
  all_t_centered <- (-hh):(bwidth_right[[1]])
  m <- length(all_t_centered)
  all_t_centered_2 <- all_t_centered^2
  
  # All u-values to be considered in the kernel functions
  all_u <- all_t_centered / bwidth_absolute
  all_u_poly <- t(unname(poly(all_u, degree = order_poly, raw = TRUE)))

  allUpB <- bwidth_absolute + bwidth_right  
  
  # Select the weighting function based on the selected kernel name
  # This way, some calculations will be faster, especially for the
  # Uniform and Epanechnikov kernels
  weight_fun <- weights_select(mu)
  
  dataMat_G <- 
  
  # for (j in 1:(order_poly + 1)) {
  #   dataMat_G[j, 1:bwidth_max] <- (((1:bwidth_total) - bwidth_absolute - 1) / bwidth_absolute)^(j - 1)
  # }

  for (j in 1:(trunc((s - 1) / 2))) {
    dataMat_S[order_poly + 2 * j, 1:bwidth_max] <- cos(2 * j * pi * (1:bwidth_max) / s)
    dataMat_S[order_poly + 2 * j + 1, 1:bwidth_max] <- sin(2 * j * pi * (1:bwidth_max) / s)
  }

  if (s / 2 == trunc(s / 2)) {
    dataMat_S[order_poly + period, 1:bwidth_max] <- cos(pi * (1:bwidth_max))
  }

  xt <- rbind(dataMat_G[1:(order_poly + 1), ], dataMat_S[(order_poly + 2):(order_poly + perid), ])

  bwidth_right <- c(2 * bwidth_absolute - (0:bwidth_absolute)) #pointwise right bandwith

  for (i in 1:(n - bwidth_absolute - 1)) {
  
    if (i <= bwidth_absolute + 1) {
    
      wk <- (1 - (((1:bwidth_total) - i) / (bwidth_right[[i]] + 0.5))^2)^mu
    
      for (j in 1:(order_poly + period)) {
        dataMat_weighted[j, ] <- xt[j, ] * wk
      }
    
      yw <- solve(dataMat_weighted %*% t(xt)) %*% dataMat_weighted
    
      xgt <- t(dataMat_G[, i])
      xst <- t(dataMat_S[, i])
    
      gw <- xgt %*% yw
      G[[i]] <- gw %*% cbind(y[1:bwidth_total])
      sw <- xst %*% yw
      S[[i]] <- sw %*% cbind(y[1:bwidth_total])
    
      G[[n - i + 1]] <- gw %*% cbind(y[n:(n - bwidth_total + 1)])
      S[[n - i + 1]] <- sw %*% cbind(y[n:(n - bwidth_total + 1)])
    
    }
  
    if (i > (h + 1) && i < (n - h)) {
      G[[i]] <- gw %*% cbind(y[(i - bwidth_absolute):(i + bwidth_absolute)])
      S[[i]] <- sw %*% cbind(y[(i - bwidth_absolute):(i + bwidth_absolute)])
    }
  }

  data.frame(trend = G, season = S)
  
}

deseats_fit <- function(spec, time_series) {
  test
}