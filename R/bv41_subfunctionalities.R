#' BV4.1 Outlier Component

outlier_detect.bv41 <- function(xt, tau = 3, M = NULL) {
  
  freq <- frequency(xt)
  stopifnot(freq %in% c(4, 12))
  
  xt <- c(xt)
  
  if (is.null(M)) {
  
    if (freq == 4) {
    
      M <- 8
    
    } else if (freq == 12) {
    
      M <- 24
    
    }
    
  }  
  
  # 1 indicates that the method should be applied forward in time
  
  # 1 also automatically calls 0 (= backward in time) and combines
  # the results
  outlierDetectBV41Cpp(xt, tau, M, 1)
  
}

### Jumping points

jump_dummy.bv41 <- function(xt, jump_points) {
  
  n <- length(xt)
  
  JumpVarsBV41Cpp(jump_points, n)
  
}
