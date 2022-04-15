bv4.1_trend <- function(yt, type = c("monthly", "quarterly")) {
  
  if (!is.na(type) && length(type) == 2 && all(type) == c("monthly", "quarterly")) {
    type = "monthly"
  }
  
  max_vals <- switch(
    type,
    monthly = 30,
    quarterly = 11
  )
  n_rows <- switch(
    type,
    monthly = 13,
    quarterly = 4
  )
  ws <- .bv4.1_ws_trend[[type]]
  
  n <- length(yt)
  ye <- rep(NA, n)
  
  N <- 2 * n_rows + 1
  
  ye[1:n_rows] <- ws[1:n_rows, ] %*% head(yt, max_vals)
  ymd <- matrix(NA, ncol = n - 2 * n_rows, nrow = N)
  for (i in (n_rows + 1):(n - n_rows)) {
    ymd[, i - n_rows] <- yt[(i - n_rows):(i + n_rows)]
  }
  ye[(n_rows + 1):(n - n_rows)] <- t(ws[(n_rows + 1), 1:N]) %*% ymd
  ye[(n - (n_rows - 1)):n] <- ws[(n_rows + 2):N, ] %*% tail(yt, max_vals)
  ye
}
