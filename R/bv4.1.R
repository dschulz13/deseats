bv4.1 <- function(yt) {
  n <- length(yt)
  ye <- rep(NA, n)
  ye[1:13] <- .ws_trend_monthly[1:13, ] %*% head(yt, 30)
  ymd <- matrix(NA, ncol = n - 26, nrow = 27)
  for (i in 14:(n - 13)) {
    ymd[, i - 13] <- yt[(i - 13):(i + 13)]
  }
  ye[14:(n - 13)] <- t(.ws_trend_monthly[14, 1:27]) %*% ymd
  ye[(n - 12):n] <- .ws_trend_monthly[15:27, ] %*% tail(yt, 30)
  ye
}

.bv4.1_weights <- function(ti, k) {
  1 - abs(ti) / (k + 1)
}

.obtain_weights_b4.1 <- function(tp_ll = numeric(0), tp_lc = -13:13, weight_ll = 0, weight_lc = 1) {
  
  b_ll = length(tp_ll)
  b_lc = length(tp_lc)  
  
  # Local cubic part
  D <- max(tp_lc)
  weights_trend <- .bv4.1_weights(ti = tp_lc, k = D)
  tt <- tp_lc / 10
  X <- matrix(1, nrow = 4, ncol = b_lc)
  X[2, ] <- tt
  X[3, ] <- tt^2
  X[4, ] <- tt^3
  XW <- X %*% diag(weights_trend)
  ws_lc <- solve(XW %*% t(X), XW)[1, ]
  
  # Local linear part
  if (b_ll > 0) {
  D <- max(tp_ll)
  weights_trend <- .bv4.1_weights(ti = tp_ll, k = D)
  tt <- tp_ll / 10
  X <- matrix(1, nrow = 2, ncol = b_ll)
  X[2, ] <- tt
  XW <- X %*% diag(weights_trend)
  ws_ll <- c(solve(XW %*% t(X), XW)[1, ], rep(0, max(0, b_lc - b_ll)))
  } else {
    ws_ll <- rep(0, b_lc)
  }

  ws <- weight_ll * ws_ll + weight_lc * ws_lc
  ws
  
}

.list_tp_ll <- vector("list", 14)
.list_tp_ll[[1]] <- 0:19
for (i in 2:7) {
  .list_tp_ll[[i]] <- c(.list_tp_ll[[i - 1]] - 1, max(.list_tp_ll[[i - 1]]))
}
for (i in 8:14) {
  .list_tp_ll[[i]] <- numeric(0)
}
.list_tp_lc <- list(
  0:26,
  -1:24,
  -2:22,
  -3:21,
  -4:20,
  -5:19,
  -6:19,
  -7:20,
  -8:20,
  -9:20,
  -10:19,
  -11:17,
  -12:15,
  -13:13
)

.weight_ll <- c(seq(6 / 12, 1 / 12, by = -(1 / 12)), rep(0, 8))
.weight_lc <- c(seq(6 / 12, 11 / 12, by = 1 / 12), rep(1, 8))

.list_ws <- purrr::pmap(
  list(
    .list_tp_ll,
    .list_tp_lc,
    .weight_ll,
    .weight_lc
  ),
  function(.x, .y, .v, .w) {.obtain_weights_b4.1(tp_ll = .x, tp_lc = .y, weight_ll = .v, weight_lc = .w)}
)

.ws_trend_monthly <- matrix(0, ncol = 30, nrow = 27)
for (i in 1:14) {
  .l <- length(.list_ws[[i]])
  .ws_trend_monthly[i, 1:.l] <- .list_ws[[i]]
}
.ws_trend_monthly[15:27, ] <- rev(.ws_trend_monthly[1:13, ])

