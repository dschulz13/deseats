#' Bootstrapping Confidence Intervals for Bandwidths
#' 
#' A stationary block bootstrap is applied to resample from a time 
#' series that was decomposed into a trend and a remainder by means of
#' data-driven local polynomial regression with automatically
#' selected bandwidth. Bandwidth re-estimation from each 
#' bootstrapped sample results in confidence bounds for the 
#' bandwidth.
#' 
#' @param nonpar_model the object with the nonparametric trend
#' estimation results returned by for example the function \code{tsmooth()}
#' of the \code{smoots} package.
#' @param K a numerical vector of length one that indicates the average 
#' block length to be drawn from the detrended series; the default is 30.
#' @param num.it a numeric vector of length one that indicates the number of
#' bootstrap iterations; the default is \code{n.it = 1000}.
#' @param parallel a logical vector of length one that indicates whether or 
#' not to employ parallel programming for the resampling and the subsequently 
#' data-driven bandwidth estimations from the bootstrapped samples; the default 
#' is \code{patrallel = TRUE}.
#' @param num.cores a numeric vector of length one that indicates the number of
#' CPU cores to use for parallel programming, if \code{parallel = TRUE}; the 
#' default is \code{num.cores = future::availableCores() - 1}.
#' 
#' @export
#' 
#' @details
#' Confidence bounds for the bandwidth in local polynomial regression 
#' for identifying the trend in a trend-stationary short-memory time 
#' series are obtained via a block bootstrap, which ensures that no 
#' specific model assumptions are required for the detrended series.
#' 
#' This function makes use of the \code{future} parallel programming 
#' framework to ensure exactly the same results regardless of whether
#' sequential or parallel programming, and then also regardless of
#' the number of workers, is employed.
#' 
#' @importFrom future availableCores plan multisession
#' @importFrom furrr future_map_dbl furrr_options
#' @importFrom purrr map
#' @importFrom smoots tsmooth
#' 
#' 
#' @examples
#' lgdp <- log(smoots::gdpUS$GDP)
#' est <- smoots::tsmooth(lgdp)
#' conf <- bwidth_confint(est, K = 30, n.it = 200, num.cores = 2)
#' conf
#' 

bwidth_confint <- function(nonpar_model, K = 30, n.it = 1000, parallel = TRUE, num.cores = future::availableCores() - 1) {
  trend <- nonpar_model$ye
  res <- nonpar_model$res
  
  if (isTRUE(parallel)) {
    oldplan <- future::plan()
    future::plan(strategy = future::multisession(), workers = num.cores)
    on.exit({
      future::plan(oldplan)
    }, add = TRUE, after = TRUE)
  }
  
  p <- 1 / K
  
  res2 <- c(res, res)
  n <- length(res)
  
  b0_all <- furrr::future_map_dbl(
    1:n.it,
    .f = function(.x, trend, res2, p, n) {
      Li <- rgeom(n, prob = p) + 1
      cLi <- cumsum(Li)
      nLi <- sum(cLi < n) + 1
      Li <- Li[1:nLi]
      Ni <- sample(1:n, size = nLi, replace = TRUE)
      
      index_sel <- unlist(
        purrr::map2(Ni, Li,
          function(.x, .y) {
            .x:(.x + .y - 1)
          }
        )
      )
      ts_new <- trend + head(res2[index_sel], n)
      smoots::tsmooth(ts_new)$b0
      
    }, trend = trend, res2 = res2, p = p, n = n,
    .options = furrr::furrr_options(seed = TRUE),
    .progress = TRUE
  )
  
  cat("", fill = TRUE)
  
  out <- list(
    conf = c(
      "estimate" = nonpar_model$b0,
      quantile(b0_all, probs = c(0.005, 0.025, 0.975, 0.995))
    ),
    bwidth_estimates = b0_all
  )
  
  out
  
}