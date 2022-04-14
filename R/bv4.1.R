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

.bv4.1_weights <- function(t, ti, k) {
  1 - abs(ti - t) / (k + 1)
}

.obtain_weights_lc <- function(tp_lc, t_lc) {
  b_lc <- length(tp_lc)
  D_lc <- max(abs(tp_lc - t_lc))
  W_lc <- .bv4.1_weights(t_lc, tp_lc, D_lc)
  
  tt <- tp_lc / 10
  
  X <- matrix(1, nrow = 4, ncol = b_lc)
  X[2, ] <- tt
  X[3, ] <- tt^2
  X[4, ] <- tt^3
  XW <- X %*% diag(W_lc)
  ws_lc <- solve(XW %*% t(X), XW)[1, ]
  
  ws_lc  
}

.obtain_weights_ll <- function(tp_ll, t_ll) {
  b_ll <- length(tp_ll)
  D_ll <- max(abs(tp_ll - t_ll))
  W_ll <- .bv4.1_weights(t_ll, tp_ll, D_ll)
  
  tt <- tp_ll / 10
  
  X <- matrix(1, nrow = 2, ncol = b_ll)
  X[2, ] <- tt
  XW <- X %*% diag(W_ll)
  ws_ll <- solve(XW %*% t(X), XW)[1, ]
 
  ws_ll  
}

# For local cubic

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
.list_t_lc <- rep(0, 14)

.mat_lc <- matrix(0, ncol = 30, nrow = 27)
for (i in 1:14) {
  .mat_lc[i, 1:length(.list_tp_lc[[i]])] <- .obtain_weights_lc(.list_tp_lc[[i]], .list_t_lc[[i]])
}

.mat_lc[15:27, ] <- .mat_lc[13:1, 30:1]

# For local linear at the boundary

.list_tp_ll <- list(
   0:19,
  -1:19,
  -2:19,
  -3:19,
  -4:19,
  -5:19
)

.list_t_ll <- c(0, -1, -2, -3, -4, -5)

.mat_ll <- matrix(0, ncol = 30, nrow = 27)
for (i in 1:6) {
  .mat_ll[i, 1:length(.list_tp_ll[[i]])] <- .obtain_weights_ll(.list_tp_ll[[i]], .list_t_ll[[i]])
}
.mat_ll[15:27, ] <- .mat_ll[13:1, 30:1]

.weight_ll <- c(seq(6 / 12, 1 / 12, by = -(1 / 12)), rep(0, 15), seq(1 / 12, 6 / 12, by = (1 / 12)))
.weight_lc <- c(seq(6 / 12, 11 / 12, by = 1 / 12), rep(1, 15), seq(11 / 12, 6 / 12, by = -(1 / 12)))

.mat_ll_weighted <- .mat_ll
for (i in 1:27) {
  .mat_ll_weighted[i, ] <- .mat_ll_weighted[i, ] * .weight_ll[[i]] 
}

.mat_lc_weighted <- .mat_lc
for (i in 1:27) {
  .mat_lc_weighted[i, ] <- .mat_lc_weighted[i, ] * .weight_lc[[i]] 
}

.ws_trend_monthly <- .mat_ll_weighted + .mat_lc_weighted

