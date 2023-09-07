n <- 1000

library(deseats)

xt <- (1:n) / n

m1 <- 13.1 + 8 * xt
m2 <- 3 * dnorm((xt - 0.75) / 0.1)
m3 <- 3 * dnorm((xt - 0.25) / 0.1)
m <- m1 + m2 + m3
plot.ts(m)

m_f <- expression(13.1 + 8 * x + 3 * dnorm((x - 0.75) / 0.1) + 3 * dnorm((x - 0.25) / 0.1))

frequ <- 4
D2 <- rep(c(0, 1, 0, 0), length.out = n)
D3 <- rep(c(0, 0, 1, 0), length.out = n)
D4 <- rep(c(0, 0, 0, 1), length.out = n)

S <- -0.4 + 0.7 * D2 + 1.1 * D3 - 0.2 * D4 

g <- m + S

ar <- 0.75
ma <- 0.5
sd_e <- 0.05

set.seed(123)
xt <- ts(g + c(arima.sim(model = list(ar = ar, ma = ma), n = n, sd = sd_e)), frequency = 4)
plot(xt)

arma_opts <- list(
  ar = ar,
  ma = ma,
  sd_e = sd_e
)



h <- hA_calc(
  m = m_f,
  arma = arma_opts,
  p = 3,
  mu = 1, 
  frequ = frequ,
  n = n,
  cb = 0.1  
)