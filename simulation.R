sd_e <- 0.04
ar <- 0.4
n <- 2400
season <- 12
order_poly <- 1
cb <- 0.05

#------------------------

t <- 1:n
xt <- t / n
m <- ts(-sin(1.45 * pi * (xt + 0.1)), frequency = season) + 2 + 2.3 * xt

a1 <- seq(0.2, 0.4, length.out = n)
a2 <- seq(0.4, 0.2, length.out = n)
a3 <- 0.3

s <- ts(rev(0.4 * sin(2 * pi * t / season) - 0.2 * cos(4 * 2 * pi * t / season) + (-0.1) * sin(4 * 2 * pi * t / season)), frequency = season)

set.seed(12345)
et <- ts(arima.sim(model = list(ar = ar), n = n, sd = sd_e), frequency = season)

Xt <- ts(m + s + et, frequency = season)

plot.ts(Xt)

#est_s <- deseats(Xt, set_options(bwidth = 0.35))

Xt_test <- ts(head(Xt, n - 10), frequency = 12)
est <- s_semiarma(Xt_test, set_options(order_poly = 3))
est2 <- BV4.1_arma(Xt_test)
fc <- predict(est, n.ahead = 10)
fc2 <- predict(est2, n.ahead = 10)
measures(fc, Xt)
measures(fc2, Xt)

#------------------------------------------------------------------------------#

cf <- (1 / (1 - sum(ar)))^2 * sd_e^2
k <- order_poly + 1
f1 <- factorial(k)^2 / (2 * k)

Kr <- function(u) {0.75 * (1 - u^2)}
W <- function(u) {0.75 * (1 - u^2)}

RK <- function(u, Kr) {Kr(u)^2}
muk <- function(u, k, Kr) {u^k * Kr(u)}

f2_num <- cf * (1 - 2 * cb) * (integrate(RK, -1, 1, Kr = Kr)[[1]] + (season - 1) * integrate(RK, -1, 1, Kr = W)[[1]])

mxt <- expression(-sin(1.45 * pi * (x + 0.1)) + 2 + 2.3 * x)

DD <- function(expression, deriv = 2, name = "x") {
  if (deriv >= 1) {
    out <- D(DD(expression, deriv = deriv - 1, name = name), name = name)
  } else {
    out <- expression
  }
  out
}

mk <- function(x) {(eval(DD(mxt, order_poly + 1)))^2}

f2_den <- integrate(mk, cb, 1 - cb)[[1]] * integrate(muk, -1, 1, k = k, Kr = Kr)[[1]]^2

hA <- (f1 * f2_num / f2_den)^(1 / (2 * k + 1)) * n^(-1 / (2 * k + 1))