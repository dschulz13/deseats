n <- 240
t <- 1:n
xt <- t / n
m <- ts(-sin(1.45 * pi * (xt + 0.1)), frequency = 12) + 2 + 2.3 * xt
s <- ts(rev(0.2 * sin(2 * pi * t / 12) + 0.2 * cos(4 * 2 * pi * t / 12) + 0.3 * sin(2 * 2 * pi * t / 12)), frequency = 12)
et <- ts(arima.sim(model = list(ar = 0.7), n = n, sd = 0.04), frequency = 12)

Xt <- ts(m + s + et, frequency = 12)

plot.ts(Xt)

#est_s <- deseats(Xt, set_options(bwidth = 0.35))

Xt_test <- ts(head(Xt, n - 10), frequency = 12)
est <- s_semiarma(Xt_test, set_options(order_poly = 3))
est2 <- BV4.1_arma(Xt_test)
fc <- predict(est, n.ahead = 10)
fc2 <- predict(est2, n.ahead = 10)
measures(fc, Xt)
measures(fc2, Xt)