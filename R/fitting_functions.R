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

  # Ensure that all observations, even time series, are transformed into
  # a simple numeric vector
  y <- as.numeric(y)
  
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
  
  # Empty matrices for the weighting systems
  ws <- matrix(0, nrow = bwidth_max, ncol = bwidth_max)
  ws_G <- ws
  ws_S <- ws

  # Empty matrix for X values
  X_G <- rep(0, nrow = order_poly + 1, ncol = bwidth_max)
  X_s <- rep(0, nrow = period - 1, ncol = bwidth_max)
  
  seq_bwidth_abs <- 0:bwidth_absolute
  right_bwidth_extension <- bwidth_absolute - seq_bwidth_abs
  bwidth_right <- bwidth_absolute + bf * right_bwidth_extension
  bwidth_right_squared <- (bwidth_right + 1)^2  
  bwidth_total <- seq_bwidth_abs + bwidth_right + 1  # NOTE: remove + 1 in C++

  # All (centered) t-values to consider for all time points (only a subset is
  # later considered for each observation time point)
  all_t_centered <- (-bwidth_absolute):(bwidth_right[[1]])
  m <- length(all_t_centered)
  all_t_centered_2 <- all_t_centered^2
  
  # All u-values to be considered in the kernel functions
  all_GX <- all_t_centered / bwidth_absolute
  all_GX_poly <- rbind(
    rep(1, m),
    t(unname(poly(all_GX, degree = order_poly, raw = TRUE)))
  )
  
  all_SX <- matrix(0, nrow = period - 1, ncol = m)
  for (i in 1:trunc((period - 1) / 2)) {
    all_SX[2 * i - 1, ] <- cos(2 * i * pi * all_t_centered / period)
    all_SX[2 * i, ] <- sin(2 * i * pi * all_t_centered / period)
  }
  
  if (period / 2 == trunc(period / 2)) {
    all_SX[period - 1, ] <- cos(pi * all_t_centered) 
  }

  allLowB <- bwidth_absolute - seq_bwidth_abs + 1   # NOTE: remove + 1 in C++
  allUpB <- bwidth_absolute + bwidth_right + 1
  
  # Select the weighting function based on the selected kernel name
  # This way, some calculations will be faster, especially for the
  # Uniform and Epanechnikov kernels
  weight_fun <- weights_select(mu)
  
  
  #xt <- rbind(dataMat_G[1:(order_poly + 1), ], dataMat_S[(order_poly + 2):(order_poly + period), ])

  Xt <- rbind(all_GX_poly, all_SX)
  
  wk <- rep(0, bwidth_max)
  
  dataMat <- matrix(0, nrow = period + order_poly, ncol = bwidth_max)
  
  ej <- t(c(1, rep(0, order_poly), rep(0, period - 1)))
  phi <- t(c(rep(0, order_poly + 1), rep(c(1, 0), period / 2)[-period]))
  vec <- t(c(ej[1:(order_poly + 1)], phi[(order_poly + 2):(order_poly + period)]))
  
  for (i in 1:(bwidth_absolute + 1)) {
  
    lowB <- allLowB[[i]]
    upB <- allUpB[[i]]
  
    wk[1:bwidth_total[[i]]] <- weight_fun(lowB = lowB, upB = upB, 
                       allsequ2 = all_t_centered_2, 
                       hr2 = bwidth_right_squared[[i]])
    
    dataMat[, 1:bwidth_total[[i]]] <- Xt[, lowB:upB]
    xw <- dataMat %*% diag(wk)
    xa <- solve(xw %*% t(dataMat), xw)
    ws_G[i, ] <- ej %*% xa
    ws_S[i, ] <- phi %*% xa
    ws[i, ] <- vec %*% xa

  }
  
  ws_G[(bwidth_absolute + 2):(bwidth_max), ] <- ws_G[bwidth_absolute:1, bwidth_max:1]
  ws_S[(bwidth_absolute + 2):(bwidth_max), ] <- ws_S[bwidth_absolute:1, bwidth_max:1]
  ws[(bwidth_absolute + 2):(bwidth_max), ] <- ws[bwidth_absolute:1, bwidth_max:1]
  
  ye_G <- rep(NA, n)
  ye_S <- ye_G
  
  ye_G[1:bwidth_absolute] <- ws_G[1:bwidth_absolute, , drop = FALSE] %*% y[1:(2 * bwidth_absolute + 1)]
  y_mat <- matrix(0, ncol = 2 * bwidth_absolute + 1, nrow = n - 2 * bwidth_absolute)
  for (i in 1:(n - 2 * bwidth_absolute)) {
    y_mat[i, ] <- y[i:(i + 2 * bwidth_absolute)]
  }
  ye_G[(bwidth_absolute + 1):(n - bwidth_absolute)] <- y_mat %*% ws_G[bwidth_absolute + 1, , drop = TRUE]
  ye_G[(n - bwidth_absolute + 1):n] <- ws_G[(bwidth_absolute + 2):(2 * bwidth_absolute + 1), , drop = FALSE] %*% y[(n - 2 * bwidth_absolute):n]
  
  ye_S[1:bwidth_absolute] <- ws_S[1:bwidth_absolute, , drop = FALSE] %*% y[1:(2 * bwidth_absolute + 1)] 
  ye_S[(bwidth_absolute + 1):(n - bwidth_absolute)] <- y_mat %*% ws_S[bwidth_absolute + 1, , drop = TRUE]
  ye_S[(n - bwidth_absolute + 1):n] <- ws_S[(bwidth_absolute + 2):(2 * bwidth_absolute + 1), , drop = FALSE] %*% y[(n - 2 * bwidth_absolute):n]  
  
  
  #cbind(Observations = y, Trend = ye_G, Season = ye_S, Residuals = y - ye_G- ye_S)
  Xt
}

deseats_fit <- function(spec, time_series) {
  test
}