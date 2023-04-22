bv_weights <- function(tp, ti, D) {
  1- abs(ti - tp) / (D + 1)
}

#### Trend

bv_trend_filter_right <- function(k, h, q, p, s) {
  D <- k - q
  tp <- 1:k
  weights <- bv_weights(tp, q, D)
  
  mat <- matrix(0, ncol = k, nrow = p + s)
  mat[1, ] <- 1
  for (i in 1:p) {
    mat[i + 1, ] <- (tp - h)^i
  }
  
  l1 <- 2 * pi / s
  
  ej <- c(1, rep(0, p), rep(0, s - 1))
  
  for (i in 1:trunc((s - 1) / 2)) {
    mat[p + 2 * i, ] <- cos(l1 * i * (tp - h))
    mat[p + 2 * i + 1, ] <- sin(l1 * i * (tp - h))
  }
  
  if ((s / 2.0) == trunc(s / 2.0)) {
    mat[p + s, ] = cos(pi * (tp - h));
  }    
  
  wmat <- mat %*% diag(weights)
  
  c(t(ej) %*% solve(wmat %*% t(mat), wmat))
  
}

bv_trend_filter_monthly <- function() {
  monthly1 <- filter_trend_list$monthly1
  monthly2 <- filter_trend_list$monthly2
  ws1 <- matrix(0, nrow = length(monthly1$k), ncol = max(monthly1$k))
  ws2 <- ws1
  
  for (i in seq_along(monthly1$k)) {
    ws1[i, c(1:monthly1$k[[i]])] <- bv_trend_filter_right(
      monthly1$k[[i]], monthly1$h[[i]], monthly1$q[[i]],
      monthly1$p[[i]], 12
    )
  }
  for (i in seq_along(monthly2$k)) {
    if (all(!is.na(c(monthly2$k[[i]], monthly2$h[[i]],
                     monthly2$q[[i]], monthly2$q[[i]])))) {
      ws2[i, 1:monthly2$k[[i]]] <- bv_trend_filter_right(
        monthly2$k[[i]], monthly2$h[[i]], monthly2$q[[i]],
        monthly2$p[[i]], 12
      )
    }
  }
  
  weighting2 <- c(seq(from = 6 / 12, to = 1 / 12, by = -1 / 12), rep(0, 8))
  weighting1 <- c(seq(from = 6 / 12, to = 11 / 12, by = 1 / 12), rep(1, 8))
  
  ws <- ws1 * weighting1 + ws2 * weighting2
  
  rbind(ws, ws[(length(monthly1$k) - 1):1, max(monthly1$k):1])
}

bv_trend_filter_quarterly <- function() {
  quarterly1 <- filter_trend_list$quarterly1
  quarterly2 <- filter_trend_list$quarterly2
  ws1 <- matrix(0, nrow = length(quarterly1$k), ncol = max(quarterly1$k))
  ws2 <- ws1
  
  for (i in seq_along(quarterly1$k)) {
    ws1[i, c(1:quarterly1$k[[i]])] <- bv_trend_filter_right(
      quarterly1$k[[i]], quarterly1$h[[i]], quarterly1$q[[i]],
      quarterly1$p[[i]], 4
    )
  }
  for (i in seq_along(quarterly2$k)) {
    if (all(!is.na(c(quarterly2$k[[i]], quarterly2$h[[i]],
                     quarterly2$q[[i]], quarterly2$q[[i]])))) {
      ws2[i, 1:quarterly2$k[[i]]] <- bv_trend_filter_right(
        quarterly2$k[[i]], quarterly2$h[[i]], quarterly2$q[[i]],
        quarterly2$p[[i]], 4
      )
    }
  }

  
  weighting2 <- rep(0.5, length(quarterly1$k))
  weighting1 <- weighting2
  weighting2[[2]] <- 0
  weighting1[[2]] <- 1
  weighting2[[5]] <- 0
  weighting1[[5]] <- 1  
  
  ws <- ws1 * weighting1 + ws2 * weighting2
  
  rbind(ws, ws[(length(quarterly1$k) - 1):1, max(quarterly1$k):1])
  
}

filter_trend_list <- list(
  monthly1 = data.frame(
    k = c(27, 26, 25, 25, 25, 25, 26, 28, 29, 30, 30, 29, 28, 27),
    h = c(1:13, 14),
    q = c(1:13, 14),
    p = c(rep(3, 14))    
  ),
  monthly2 = data.frame(
    k = c(20:25),
    h = c(1:6),
    q = c(rep(1, 6)),
    p = c(rep(1, 6))    
  ),
  quarterly1 = data.frame(
    k = c(10, 9, 10, 11, 9),
    h = c(1:5),
    q = c(1:5),
    p = c(rep(3, 5))    
  ),
  quarterly2 = data.frame(
    k = c(8, NA, 9, 10, NA),
    h = c(1, NA, 3, 4, NA),
    q = c(1, NA, 3, 4, NA),
    p = c(1, NA, 3, 3, NA)    
  )  
)

#### Seasonality

filter_season_list <- list(
  monthly1 = data.frame(
    k = c(24:47),
    h = c(1:24),
    q = rev(c(
      24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 18, 17, 17, 16, 16, 15, 15, 
      14, 12, 10, 8, 6
    )),
    p = c(rep(1, 12), rep(3, 12)),
    weights = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 2 / 3, 2 / 3, 4 / 5, 4 / 5,
                0.75, 0.75, 1 / 9, 2 / 9, 0.5, 0.5, 0.5, 0.5, 2 / 3, 2 / 3, 
                4 / 5, 4 / 5, 6 / 7, 6 / 7)
  ),
  monthly2 = data.frame(
    k = c(36:49, 38:45, 47, 47),
    h = c(1:22, 24, 24),
    q = c(13, 11, 9, 7, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 19, 20, 20, 
          21, 21, 22, 22, 23, 24, 24),
    p = c(rep(3, 14), rep(1, 10)),
    weights = c(0.25, 0.25, 0.25, 1 / 3, 0.5, 0.5, 1 / 3, 1 / 3, 1 / 5, 1 / 5,
                1 / 6, 1 / 6, 2 / 9, 1 / 9, 0.5, 0.5, 0.5, 0.5, 1 / 3, 1 / 3,
                1 / 5, 1 / 5, 1 / 7, 1 / 7)
  ),
  monthly3 = data.frame(
    k = c(48:51, rep(NA, 6), 34:37, rep(NA, 10)),
    h = c(1:4, rep(NA, 6), 11:14, rep(NA, 10)),
    q = c(13, 11, 9, 7, rep(NA, 6), 11, 12, 18, 19, rep(NA, 10)),
    p = c(rep(3, 4), rep(NA, 6), 3, 3, 1, 1, rep(NA, 10)),
    weights = c(0.25, 0.25, 0.25, 1 / 6, rep(0, 6), 1 / 12, 1 / 12, 2 / 3, 
                2 / 3, rep(0, 10))
  ),

  quarterly1 = data.frame(
    k = c(8:15),
    h = c(1:8),
    q = c(2, 5, 5, 6, 6, 7, 7, 8),
    p = c(rep(1, 8)),
    weights = c(0.5, 2 / 5, 4 / 5, 4 / 5, 0.5, 0.5, 0.5, 0.5)
  ),
  quarterly2 = data.frame(
    k = c(12:16, 13:15),
    h = c(1:8),
    q = c(4, 4, 5, 5, 6, 6, 7, 8),
    p = c(rep(3, 8)),
    weights = c(1 / 5, 2 / 5, 1 / 5, 1 / 5, 3 / 10, 0.5, 0.5, 0.5)
  ),
  quarterly3 = data.frame(
    k = c(16, 17, rep(NA, 2), 12, rep(NA, 3)),
    h = c(1, 2, NA, NA, 5, NA, NA, NA),
    q = c(3, 3, NA, NA, 5, NA, NA, NA),
    p = c(3, 3, NA, NA, 3, NA, NA, NA),
    weights = c(3 / 10, 1 / 5, 0, 0, 1 / 5, 0, 0, 0)
  )    
)

bv_season_filter_right <- function(k, h, q, p, s) {
  D <- k - q
  tp <- 1:k
  weights <- bv_weights(tp, q, D)
  
  mat <- matrix(0, ncol = k, nrow = p + s)
  mat[1, ] <- 1
  for (i in 1:p) {
    mat[i + 1, ] <- (tp - h)^i
  }
  
  l1 <- 2 * pi / s
  
  phi <- c(rep(0, p + 1), rep(c(1, 0), trunc((s - 1) / 2)), 1)
  
  for (i in 1:trunc((s - 1)/ 2)) {
    mat[p + 2 * i, ] <- cos(l1 * i * (tp - h))
    mat[p + 2 * i + 1, ] <- sin(l1 * i * (tp - h))
  }
  
  if ((s / 2.0) == trunc(s / 2.0)) {
    mat[p + s, ] = cos(pi * (tp - h));
  }    
  
  wmat <- mat %*% diag(weights)
  
  c(t(phi) %*% solve(wmat %*% t(mat), wmat))
  
}

bv_season_filter_monthly <- function() {
  monthly1 <- filter_season_list$monthly1
  monthly2 <- filter_season_list$monthly2
  monthly3 <- filter_season_list$monthly3  
  
  maxi <- max(monthly3$k[!is.na(monthly3$k)])
  
  ws1 <- matrix(0, nrow = length(monthly1$k), ncol = maxi)
  ws2 <- ws1
  ws3 <- ws1
  
  for (i in seq_along(monthly1$k)) {
    ws1[i, c(1:monthly1$k[[i]])] <- bv_season_filter_right(
      monthly1$k[[i]], monthly1$h[[i]], monthly1$q[[i]],
      monthly1$p[[i]], 12
    )
  }
  for (i in seq_along(monthly2$k)) {
    if (all(!is.na(c(monthly2$k[[i]], monthly2$h[[i]],
                     monthly2$q[[i]], monthly2$q[[i]])))) {
      ws2[i, 1:monthly2$k[[i]]] <- bv_season_filter_right(
        monthly2$k[[i]], monthly2$h[[i]], monthly2$q[[i]],
        monthly2$p[[i]], 12
      )
    }
  }
  for (i in seq_along(monthly3$k)) {
    if (all(!is.na(c(monthly3$k[[i]], monthly3$h[[i]],
                     monthly3$q[[i]], monthly3$q[[i]])))) {
      ws3[i, 1:monthly3$k[[i]]] <- bv_season_filter_right(
        monthly3$k[[i]], monthly3$h[[i]], monthly3$q[[i]],
        monthly3$p[[i]], 12
      )
    }
  }  
  
  ws <- ws1 * monthly1$weights + ws2 * monthly2$weights + ws3 * monthly3$weights
  
  rbind(ws, ws[(length(monthly1$k) - 1):1, maxi:1])
}


bv_season_filter_quarterly <- function() {
  quarterly1 <- filter_season_list$quarterly1
  quarterly2 <- filter_season_list$quarterly2
  quarterly3 <- filter_season_list$quarterly3  
  
  maxi <- max(quarterly3$k[!is.na(quarterly3$k)])
  
  ws1 <- matrix(0, nrow = length(quarterly1$k), ncol = maxi)
  ws2 <- ws1
  ws3 <- ws1
  
  for (i in seq_along(quarterly1$k)) {
    ws1[i, c(1:quarterly1$k[[i]])] <- bv_season_filter_right(
      quarterly1$k[[i]], quarterly1$h[[i]], quarterly1$q[[i]],
      quarterly1$p[[i]], 4
    )
  }
  for (i in seq_along(quarterly2$k)) {
    if (all(!is.na(c(quarterly2$k[[i]], quarterly2$h[[i]],
                     quarterly2$q[[i]], quarterly2$q[[i]])))) {
      ws2[i, 1:quarterly2$k[[i]]] <- bv_season_filter_right(
        quarterly2$k[[i]], quarterly2$h[[i]], quarterly2$q[[i]],
        quarterly2$p[[i]], 4
      )
    }
  }
  for (i in seq_along(quarterly3$k)) {
    if (all(!is.na(c(quarterly3$k[[i]], quarterly3$h[[i]],
                     quarterly3$q[[i]], quarterly3$q[[i]])))) {
      ws3[i, 1:quarterly3$k[[i]]] <- bv_season_filter_right(
        quarterly3$k[[i]], quarterly3$h[[i]], quarterly3$q[[i]],
        quarterly3$p[[i]], 4
      )
    }
  }  
  
  ws <- ws1 * quarterly1$weights + ws2 * quarterly2$weights + ws3 * quarterly3$weights
  
  rbind(ws, ws[(length(quarterly1$k) - 1):1, maxi:1])
}

#### Combine

bv_mats <- list(
  monthly = list(
    trend = bv_trend_filter_monthly(),
    season = bv_season_filter_monthly()
  ),
  quarterly = list(
    trend = bv_trend_filter_quarterly(),
    season = bv_season_filter_quarterly()    
  )
)
