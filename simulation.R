# Settings to check

trend_FUNs <- list(
  m1 = expression(-sin(1.45 * pi * (x + 0.1)) + 2 + 2.3 * x + exp(1.2 * x)),
  m2 = expression(2 + 4 * x + sin(2 * pi * (x - 0.5) / 1.5)),
  m3 = expression(0 * x + 10),
  m4 = expression(1.2 + 10.1 * x)
)

season_FUNs <- list(
  s1 = expression(rev(0.7 * sin(2 * pi * t / season) - 0.5 * cos(4 * 2 * pi * t / season) + (-0.4) * sin(4 * 2 * pi * t / season))),
  s2 = expression(2 * (0.04 * sin(2 * pi * (t - pi - 1.2) / season) + 0.005 * sin(2 * pi * (t - 0.5 * pi - 1.2) / season)  + 0.025 * sin(4 * pi * (t - 1.2) / season))),
  s3 = expression(0 * x)
) 

arma_settings <- list(
  xi1 = list(ar = 0.4, ma = 0, sd = 0.04),
  xi2 = list(ar = 0.7, ma = 0.65, sd = 0.02),
  xi3 = list(ar = 0.8, ma = -0.2, sd = 0.01),
  xi4 = list(ar = NULL, ma = NULL, sd = 0.1)
)

nobs_settings <- list(
  n1 = 250,
  n2 = 500,
  n3 = 750,
  n4 = 1000
)

season <- 12
order_poly <- 3
cb <- 0.05

#-------------------------------

sim_s_semiarma <- function(trend_fun, season_fun, arma_model = list(ar = 0.5, ma = 0.4, sd = 0.04), season = 12, n = season * 10 * 5) {
  
  ar <- arma_model$ar
  ma <- arma_model$ma
  sd <- arma_model$sd
  t <- 1:n
  x <- t / n
  m <- ts(eval(trend_fun), frequency = season)  
  s <- ts(eval(season_fun), frequency = season)
  et <- ts(arima.sim(model = list(ar = ar, ma = ma), n = n, sd = sd), frequency = season)
  Xt <- ts(m + s + et, frequency = season)
  out <- cbind(
    "Observations" = Xt,
    "Trend" = m,
    "Season" = s,
    "Rest" = et
  )
  out
}

sim_bwidth <- function(trend_fun, season_fun, arma_model = list(ar = 0.5, ma = 0.4, sd = 0.04), season = 12, n = season * 10 * 5, order_poly) {

    t <- 1:n
    x <- t / n
    m <- ts(eval(trend_fun), frequency = season)  
    s <- ts(eval(season_fun), frequency = season)
    ar <- arma_model$ar
    ma <- arma_model$ma
    sd_e <- arma_model$sd
    force(order_poly)
    
    function(i) {
      
      et <- ts(arima.sim(model = list(ar = ar, ma = ma), n = n, sd = sd_e), frequency = season)
      Xt <- m + s + et
      deseats::deseats(Xt, set_options(order_poly = order_poly))@bwidth
      
    }
   
}  

set.seed(123456)
ts_sim <- sim_s_semiarma(trend_FUNs[["m1"]], season_FUNs[["s2"]], arma_model = arma_settings[["xi2"]], season = season, n = nobs_settings[["n4"]])
plot(ts_sim)

#est_s <- deseats(Xt, set_options(bwidth = 0.35))

# Xt_test <- ts(head(Xt, n - 10), frequency = 12)
# est <- s_semiarma(Xt_test, set_options(order_poly = 3))
# est2 <- BV4.1_arma(Xt_test)
# fc <- predict(est, n.ahead = 10)
# fc2 <- predict(est2, n.ahead = 10)
# measures(fc, Xt)
# measures(fc2, Xt)

#------------------------------------------------------------------------------#

hA_calc <- function(trend_fun, order_poly = 1, season = 12, mu = 1, arma_model = list(ar = 0.5, ma = 0.4, sd = 0.04), cb = 0.05, n = n) {

  ar <- arma_model$ar
  ma <- arma_model$ma
  var_e <- arma_model$sd^2
  cf <- ((1 + sum(ma)) / (1 - sum(ar)))^2 * var_e
  
  k <- order_poly + 1
  f1 <- factorial(k)^2 / (2 * k)

  if (order_poly == 1) {
  
    K_trend <- function(u) {0.75 * (1 - u^2)}
    
  } else if (order_poly == 3) {
    
    K_trend <- switch(as.character(mu),
      "0" = function(u) {3 / 8 * (3 - 5 * u^2)},
      "1" = function(u) {15 / 32 * (3 - 10 * u^2 + 7 * u^4)},
      "2" = function(u) {105 / 64 * (1 - 5 * u^2 + 7 * u^4 - 3 * u^6)},
      "3" = function(u) {315 / 512 * (3 - 20 * u^2 + 42 * u^4 - 36 * u^6 + 11 * u^8)}
    )
    
  }
    
  K_season <- function(u) {0.75 * (1 - u^2)}

  RK <- function(u, K) {K(u)^2}
  muk <- function(u, k, K) {u^k * K(u)}

  f2_num <- cf * (1 - 2 * cb) * (integrate(RK, -1, 1, K = K_trend)[[1]] + (season - 1) * integrate(RK, -1, 1, K = K_season)[[1]])

  DD <- function(expression, deriv = 2, name = "x") {
    if (deriv >= 1) {
      out <- D(DD(expression, deriv = deriv - 1, name = name), name = name)
    } else {
      out <- expression
    }
    out
  }

  mk <- function(x, trend_fun, order_poly) {(eval(DD(trend_fun, order_poly + 1)))^2}

  Imk <- tryCatch(
    {integrate(mk, cb, 1 - cb, trend_fun = trend_fun, order_poly = order_poly)[[1]]},
    error = function(c1) {0}
  )
  
  f2_den <- Imk * integrate(muk, -1, 1, k = k, K = K_trend)[[1]]^2

  hA <- (f1 * f2_num / f2_den)^(1 / (2 * k + 1)) * n^(-1 / (2 * k + 1))
  if (is.infinite(hA)) {
    hA <- 0.49      # For a constant trend the best we can do is the highest possible bandwidth of 0.49
  }
  hA
}

hA <- hA_calc(trend_FUNs[["m1"]], order_poly, season, mu = 1, arma_settings[["xi2"]], cb = cb, n = nobs_settings[["n4"]])

hA

#----------------------------------------------------------------

sim_fun <- sim_bwidth(trend_fun, season_fun, list(ar = ar, ma = ma, sd = sd_e), season, n, order_poly)

library(future)
library(furrr)

ncor <- availableCores() - 1

oldplan <- plan()
plan(multisession, workers = ncor)

set.seed(123)

bwidths <- future_map_dbl(
  1:1000,
  sim_fun,
  .options = furrr_options(seed = TRUE),
  .progress = TRUE
)

plan(oldplan)

hist(bwidths, freq = FALSE)
abline(v = hA, col = "red")
