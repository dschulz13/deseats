#'@export
lm_decomp <- function(yt, order_poly = 1, season = NULL) {

  ts_name <- deparse(substitute(yt))
  if (is.null(season)) {
    season <- frequency(yt)
  }
  
  N <- length(yt)
  season_add_col <- if (season > 1) {season - 1} else {0}
  X_mat <- matrix(0, nrow = N, ncol = order_poly + season_add_col)
  if (season > 1) {
    diag_N <- floor(N / season)
    rest_rows <- N - diag_N * season
    for (i in 1:diag_N) {
      X_mat[(1:season) + ((i - 1) * season), 
            (1 + order_poly):(order_poly + season - 1)] <- diag(nrow = season, 
                                                              ncol = season - 1)
    }
    if (rest_rows >= 1) {
      X_mat[(N - rest_rows + 1):N, 
          (1 + order_poly):(order_poly + season - 1)] <- diag(nrow = rest_rows, 
                                                              ncol = season - 1)
    }
    X_mat[(1:N) %% season == 0, ] <- rep(-1, order_poly + season - 1)
    name_season <- paste0("S", 1:(season - 1))
  } else {
    name_season <- c()
  }
  if (order_poly > 0) {
    t <- seq_along(yt)
    for (i in 1:order_poly) {
      X_mat[, i] <- t^i
    }
    name_trend <- paste0("t", 1:order_poly)
  } else {
    name_trend <- c()
  }
  X_df <- as.data.frame(cbind(
    as.numeric(yt),
    X_mat
    ))
  names(X_df) <- c("yt", name_trend, name_season)
  reg <- stats::lm(yt ~ ., data = X_df)

  # if (season > 1) {
    s <- reg$coefficients[(2 + order_poly):(order_poly + season)]
    seasonality <- utils::head(rep(c(s, -sum(s)), diag_N + 1), N)
    remainder <- yt - seasonality
    name_s <- "Season"
  # } else {
  #   seasonality <- c()
  #   name_s <- seasonality
  #   remainder <- yt
  # }
  # if (order_poly > 1) {
    # if (season > 1) {
      X_df[, (2 + order_poly):(1 + order_poly + season_add_col)] <- 0
    # }
    trend <- unname(predict(reg, X_df))
    remainder <- remainder - trend
    name_t <- "Trend"
  # } else {
  #   trend <- c()
  #   name_t <- trend
  # }
  
  out <- create_lmdecomp(
    decomp = cbind("Observations" = yt, "Trend" = trend, 
          "Season" = seasonality, "Rest" = remainder),
    ts_name = ts_name,
    frequency = season,
    regression_output = reg
  )

  out
}