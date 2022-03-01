I2_calc <- function(g, n, n1) {
  sum(g[(n1 + 1):(n - n1)]^2) / (n - 2 * n1)
}

Rp_calc <- function(kernel_fun) {
  
}