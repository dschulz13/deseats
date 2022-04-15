
#================================= TREND ======================================#

### Weighting function ###

## Explanation: Let t_hat be the point at which to estimate the trend

## ti: The time points to consider for smoothing (t_hat already subtracted)
## t:  Time point (t_hat already subtracted) with the most weight (= 1)
## k:  The distance from t to the right boundary of the smoothing window
##     (in number of months or quarters)

.bv4.1_weights <- function(t, ti, k) {
  1 - abs(ti - t) / (k + 1)
}

### Obtain final smoothing weights for smoothing at the time point t_hat
### (local cubic)

## tp_lc: The time points (t_hat already subtracted) to consider
## t_lc:  The time point (t_hat already subtracted) to give the most weight to

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

### Obtain final smoothing weights for smoothing at the time point t_hat
### (local linear)

## tp_ll: The time points (t_hat already subtracted) to consider
## t_ll:  The time point (t_hat already subtracted) to give the most weight to

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

#------------------------------------------------------------------------------#

#### For monthly data

### A list of all the time points (t_hat already subtracted) to consider
### (local cubic)

.list_tp_lc <- list(
  0:26,        # t_hat = 1
  -1:24,       # t_hat = 2
  -2:22,       #   .
  -3:21,       #   .
  -4:20,       #   .
  -5:19,
  -6:19,
  -7:20,
  -8:20,
  -9:20,
  -10:19,
  -11:17,
  -12:15,      # t_hat = 13
  -13:13       # t_hat = 14 (first middle point)
)
.list_t_lc <- rep(0, 14) # in local cubic: always give the most weight to t_hat

### Obtain the weighting matrix for the local cubic part

## For t_hat = 1, 2, ..., 14

.mat_lc <- matrix(0, ncol = 30, nrow = 27)
for (i in 1:14) {
  .mat_lc[i, 1:length(.list_tp_lc[[i]])] <- .obtain_weights_lc(.list_tp_lc[[i]], .list_t_lc[[i]])
}

## Revert the weights for t_hat = 1, 2, ..., 13 for the last 13 time points

.mat_lc[15:27, ] <- .mat_lc[13:1, 30:1]

### A list of all the time points (t_hat already subtracted) to consider
### (local linear)

.list_tp_ll <- list(
   0:19,    # t_hat = 1
  -1:19,    # t_hat = 2
  -2:19,    #    .
  -3:19,    #    .
  -4:19,    #    .
  -5:19     # t_hat = 6
)

# in local cubic: always give the most weight to the most left point

.list_t_ll <- c(0, -1, -2, -3, -4, -5)

### Obtain the weighting matrix for the local cubic part

## For t_hat = 1, 2, ..., 14

.mat_ll <- matrix(0, ncol = 30, nrow = 27)
for (i in 1:6) {
  .mat_ll[i, 1:length(.list_tp_ll[[i]])] <- .obtain_weights_ll(.list_tp_ll[[i]], .list_t_ll[[i]])
}
.mat_ll[15:27, ] <- .mat_ll[13:1, 30:1]

### At the points where both local cubic and local linear estimates were obtained:

### weight the weights (in the middle only local cubic is used)

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

### Sum up both weighting matrices to get the final weighting system

.ws_trend_monthly <- .mat_ll_weighted + .mat_lc_weighted

#------------------------------------------------------------------------------#

### Same procedure for quarterly data with slightly adjusted filters

### A list of all the time points (t_hat already subtracted) to consider
### (local cubic)

.list_tp_lc <- list(
   0:9,        # t_hat = 1
  -1:7,        # t_hat = 2
  -2:7,        #   .
  -3:7,        # t_hat = 4
  -4:4         # t_hat = 5 (first middle point)
)
.list_t_lc <- rep(0, 5) # in local cubic: always give the most weight to t_hat

### Obtain the weighting matrix for the first local cubic part

## For t_hat = 1, 2, ..., 5

.mat_lc <- matrix(0, ncol = 11, nrow = 9)
for (i in 1:5) {
  .mat_lc[i, 1:length(.list_tp_lc[[i]])] <- .obtain_weights_lc(.list_tp_lc[[i]], .list_t_lc[[i]])
}

.list_tp_lc_2 <- list(
  -2:6,      # t_hat = 3
  -3:6       # t_hat = 4
)

.list_t_lc_2 <- rep(0, 2) # in local cubic: always give the most weight to t_hat

.mat_lc_2 <- matrix(0, ncol = 11, nrow = 2)
for (i in 1:2) {
  .mat_lc_2[i, 1:length(.list_tp_lc_2[[i]])] <- .obtain_weights_lc(.list_tp_lc_2[[i]], .list_t_lc_2[[i]])
}

.mat_lc[3, ] <- 0.5 * .mat_lc[3, ] + 0.5 * .mat_lc_2[1, ]
.mat_lc[4, ] <- 0.5 * .mat_lc[4, ] + 0.5 * .mat_lc_2[2, ]

## Revert the weights for t_hat = 1, 2, ..., 4 for the last 4 time points

.mat_lc[6:9, ] <- .mat_lc[4:1, 11:1]

### A list of all the time points (t_hat already subtracted) to consider
### (local linear)

.list_tp_ll <- list(
   0:7    # t_hat = 1
)

# in local cubic: always give the most weight to the most left point

.list_t_ll <- c(0)

### Obtain the weighting matrix for the local cubic part

## For t_hat = 1, 2, ..., 4

.mat_ll <- matrix(0, ncol = 11, nrow = 2)
for (i in 1:1) {
  .mat_ll[i, 1:length(.list_tp_ll[[i]])] <- .obtain_weights_ll(.list_tp_ll[[i]], .list_t_ll[[i]])
}
.mat_ll[2, ] <- .mat_ll[1, 11:1]

### Sum up both weighting matrices to get the final weighting system

.ws_trend_quarterly <- .mat_lc
.ws_trend_quarterly[1, ] <- 0.5 * .ws_trend_quarterly[1, ] + 0.5 * .mat_ll[1, ]
.ws_trend_quarterly[9, ] <- 0.5 * .ws_trend_quarterly[9, ] + 0.5 * .mat_ll[2, ]

#------------------------------------------------------------------------------#

.bv4.1_ws_trend <- list(
  monthly = .ws_trend_monthly,
  quarterly = .ws_trend_quarterly
)

#usethis::use_data(.bv4.1_ws_trend, internal = TRUE, overwrite = TRUE)


#============================= SEASONALITY ====================================#

