#'@export
hamilton_filter <- function(yt, h = NULL, p = NULL) {

  ts_name <- deparse(substitute(yt))
  
  s <- stats::frequency(yt)
  h <- 2 * s
  p <- s
  
  n <- length(yt)
  
  t <- c(stats::time(yt))
  t_start <- utils::tail(t, n - (h + p - 1))[[1]]
  
  # The dependent variable for OLS
  X.dep <- utils::tail(yt, n - h - (p - 1))
  
  # Create a matrix with all independent variables
  X.ind <- matrix(NA, ncol = p, nrow = length(X.dep))
  for (i in 1:p) {
    X.ind[, i] <- yt[(p - (i - 1)):(n - h - (i - 1))]
  }
  
  # Combine the dependent and independent variables and create 
  # a dataframe with specific column names
  dataset <- as.data.frame(cbind(X.dep, X.ind))
  colnames(dataset) <- c("X.dep", paste0("X.ind", 1:p))
  
  # Conduct OLS and retrieve the coefficients, the fitted values and
  # the residuals
  est <- stats::lm(X.dep ~ ., data = dataset)
  coefs <- est[["coefficients"]]

  fit <- stats::ts(est[["fitted.values"]], start = t_start, frequency = s)
  res <- stats::ts(est[["residuals"]], start = t_start, frequency = s)
  
  # Create and return the output
  out <- create_hfilter(
    decomp = cbind(Observations = yt, Fitted = fit, Rest = res),
    ts_name = ts_name,
    frequency = s,
    regression_output = est
  )
  out  
  
}