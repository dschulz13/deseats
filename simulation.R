sd_e <- 0.04
ar <- 0.4
ma <- 0
n <- 480
season <- 12
order_poly <- 1
cb <- 0.05
trend_fun <- expression(-sin(1.45 * pi * (x + 0.1)) + 2 + 2.3 * x + exp(1.2 * x))
season_fun <- expression(rev(0.7 * sin(2 * pi * t / season) - 0.5 * cos(4 * 2 * pi * t / season) + (-0.4) * sin(4 * 2 * pi * t / season)))

#------------------------

sd_e <- 0.0086
ar <- 0.7
ma <- -0.1
n <- 480
season <- 12
order_poly <- 1
cb <- 0.05
trend_fun <- expression(2 + 4 * x + sin(2 * pi * (x - 0.5) / 1.5))
season_fun <- expression(0.04 * sin(2 * pi * (t - pi - 1.2) / 12) + 0.005 * sin(2 * pi * (t - 0.5 * pi - 1.2) / 12)  + 0.025 * sin(4 * pi * (t - 1.2) / 12))

#----------------------------------

sim_s_semiarma <- function(trend_fun, season_fun, arma_model = list(ar = 0.5, ma = 0.4, sd = 0.04), season = 12, n = season * 10 * 5) {
  
  t <- 1:n
  x <- t / n
  m <- ts(eval(trend_fun), frequency = season)  
  s <- ts(eval(season_fun), frequency = season)
  et <- ts(arima.sim(model = list(ar = ar, ma = ma), n = n, sd = sd_e), frequency = season)
  Xt <- ts(m + s + et, frequency = season)
  out <- cbind(
    "Observations" = Xt,
    "Trend" = m,
    "Season" = s,
    "Rest" = et
  )
  out
}

sim_bwidth <- function(trend_fun, season_fun, arma_model = list(ar = 0.5, ma = 0.4, sd = 0.04), season = 12, n = season * 10 * 5) {

    t <- 1:n
    x <- t / n
    m <- ts(eval(trend_fun), frequency = season)  
    s <- ts(eval(season_fun), frequency = season)
    ar <- arma_model$ar
    ma <- arma_model$ma
    sd_e <- arma_model$sd
    
    function(i) {
      
      et <- ts(arima.sim(model = list(ar = ar, ma = ma), n = n, sd = sd_e), frequency = season)
      Xt <- m + s + et
      deseats::deseats(Xt)@bwidth
      
    }
   
}  

set.seed(12345)
ts_sim <- sim_s_semiarma(trend_fun, season_fun, arma_model = list(ar = ar, ma = ma, sd = sd_e), season = season, n = n)
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

hA_calc <- function(trend_fun, order_poly = 1, season = 12, arma_model = list(ar = 0.5, ma = 0.4, sd = 0.04), cb = 0.05) {

  ar <- arma_model$ar
  ma <- arma_model$ma
  var_e <- arma_model$sd^2
  cf <- ((1 + sum(ma)) / (1 - sum(ar)))^2 * var_e
  
  k <- order_poly + 1
  f1 <- factorial(k)^2 / (2 * k)

  K_trend <- function(u) {0.75 * (1 - u^2)}
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

  f2_den <- integrate(mk, cb, 1 - cb, trend_fun = trend_fun, order_poly = order_poly)[[1]] * integrate(muk, -1, 1, k = k, K = K_trend)[[1]]^2

  (f1 * f2_num / f2_den)^(1 / (2 * k + 1)) * n^(-1 / (2 * k + 1))
  
}

hA <- hA_calc(trend_fun, order_poly, season, list(ar = ar, ma = ma, sd = sd_e), cb = cb)
hA

#----------------------------------------------------------------

sim_fun <- sim_bwidth(trend_fun, season_fun, list(ar = ar, ma = ma, sd = sd_e), season, n)

library(future)
library(furrr)

ncor <- availableCores() - 1

oldplan <- plan()
plan(multisession, workers = ncor)

set.seed(123)

bwidths <- future_map_dbl(
  1:3000,
  sim_fun,
  .options = furrr_options(seed = TRUE),
  .progress = TRUE
)

plan(oldplan)

hist(bwidths, freq = FALSE)
abline(v = hA, col = "red")
