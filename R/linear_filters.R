#' AR Representation of an ARMA Model
#' 
#' Output has representation with positive signs (on the right-hand side of the equation); inputs are both with positive signs (on right-hand side of equation).
#' 
#' @export
#' 

arma_to_ar <- function(ar = numeric(0), ma = numeric(0), max_i = 1000) {
  c(ARinftySHORT(ar = ar, ma = ma, max_i = max_i))
}

#' MA Representation of an ARMA Model
#' 
#' Output has representation with positive signs (on the right-hand side of the equation); inputs are both with positive signs (on right-hand side of equation). 
#' 
#' @export
#' 
#' @importFrom Rcpp sourceCpp
#' 

arma_to_ma <- function(ar = numeric(0), ma = numeric(0), max_i = 1000) {
  c(MAinftySHORT(ar = ar, ma = ma, max_i = max_i))
}
