mu_select <- function(kernel_fun) {
  switch(kernel_fun,
    "Uniform" = 0,
    "Epanechnikov" = 1,
    "Bisquare" = 2,
    "Triweight" = 3
  )
}

smooth_deseats <- function(y, period, order_poly, kernel_fun, bwidth, boundary_method) {

  n <- length(y)           ## number of observations

  G <- rep(0, n)
  S <- rep(0, n)
  bwidth_absolute <- trunc(n * bwidth + 0.5)
  bwidth_total <- 2 * bwidth_absolute + 1              ## total bandwidth

  mu <- mu_select(kernel_fun)

  dataMat_G <- matrix(0, order_poly + period, bwidth_total)   ## data matrix for the trend
  dataMat_S <- xg                 ## data matrix for the season
  dataMat_weighted <- dataMat_G                 ## weighted data matrix


  for (j in 1:(order_poly + 1)){
    dataMat_G[j, 1:bwidth_total] <- (((1:bwidth_total) - bwidth_absolute - 1) / bwidth_absolute)^(j - 1)
  }

  for (j in 1:(trunc((s - 1) / 2))) {
    dataMat_S[order_poly + 2 * j, 1:bwidth_total] <- cos(2 * j * pi * (1:bwidth_total) / s)
    dataMat_S[order_poly + 2 * j + 1, 1:bwidth_total] <- sin(2 * j * pi * (1:bwidth_total) / s)
  }

  if (s / 2 == trunc(s / 2)) {
    dataMat_S[order_poly + period, 1:bwidth_total] <- cos(pi * (1:bwidth_total))
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