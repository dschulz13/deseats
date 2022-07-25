#'@export
hamilton_filter <- function(yt, h = NULL, p = NULL) {

  s <- frequency(yt)
  h <- 2 * s
  p <- s
  
  n <- length(yt)
  
  t <- c(time(yt))
  t_start <- tail(t, n - (h + p - 1))[[1]]
  
  # The dependent variable for OLS
  X.dep <- tail(yt, n - h - (p - 1))
  
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
  est <- lm(X.dep ~ ., data = dataset)
  coefs <- est[["coefficients"]]

  fit <- ts(est[["fitted.values"]], start = t_start, frequency = s)
  res <- ts(est[["residuals"]], start = t_start, frequency = s)
  
  # Create and return the output
  out <- list(coefs = coefs, fitted = fit, resid = res, model = est, h = h, p = p)
  out  
  
}