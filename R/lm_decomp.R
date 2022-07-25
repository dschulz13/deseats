#'@export
lm_decomp <- function(yt, trend_poly = 1, season = 0) {

  N <- length(yt)
  season_add_col <- if (season > 1) {season - 1} else {0}
  X_mat <- matrix(0, nrow = N, ncol = trend_poly + season_add_col)
  if (season > 1) {
    diag_N <- floor(N / season)
    rest_rows <- N - diag_N * season
    for (i in 1:diag_N) {
      X_mat[(1:season) + ((i - 1) * season), 
            (1 + trend_poly):(trend_poly + season - 1)] <- diag(nrow = season, 
                                                              ncol = season - 1)
    }
    if (rest_rows >= 1) {
      X_mat[(N - rest_rows + 1):N, 
          (1 + trend_poly):(trend_poly + season - 1)] <- diag(nrow = rest_rows, 
                                                              ncol = season - 1)
    }
    X_mat[(1:N) %% season == 0, ] <- rep(-1, trend_poly + season - 1)
    name_season <- paste0("S", 1:(season - 1))
  } else {
    name_season <- c()
  }
  if (trend_poly > 0) {
    t <- seq_along(yt)
    for (i in 1:trend_poly) {
      X_mat[, i] <- t^i
    }
    name_trend <- paste0("t", 1:trend_poly)
  } else {
    name_trend <- c()
  }
  X_df <- as.data.frame(cbind(
    as.numeric(yt),
    X_mat
    ))
  names(X_df) <- c("yt", name_trend, name_season)
  reg <- stats::lm(yt ~ ., data = X_df)
  out_mat <- 
  if (season > 1) {
    s <- reg$coefficients[(2 + trend_poly):(trend_poly + season)]
    seasonality <- utils::head(rep(c(s, -sum(s)), diag_N + 1), N)
    remainder <- yt - seasonality
    name_s <- "Season"
  } else {
    seasonality <- c()
    name_s <- seasonality
    remainder <- yt
  }
  if (trend_poly > 1) {
    if (season > 1) {
      X_df[, (2 + trend_poly):(1 + trend_poly + season_add_col)] <- 0
    }
    trend <- unname(predict(reg, X_df))
    remainder <- remainder - trend
    name_t <- "Trend"
  } else {
    trend <- c()
    name_t <- trend
  }
  out <- cbind(yt, trend, seasonality, remainder)
  colnames(out) <- c("Observations", name_t, name_s, "Remainder")
  list(series = out, reg_results = reg)
}