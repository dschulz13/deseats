# p <- 3
# 
# 
# mu <- 2
# 
# s <- 4
  
CF_calc <- function(p, mu, s) {

k <- p + 1  
  
Ku <- function(u, mu) {
  (15 / 16) * (1 - u^2)^mu
}

Np_f <- function(u, i, j, mu) {
  u^{i + j - 2} * Ku(u, mu)
}

Np <- matrix(NA, ncol = k, nrow = k)

for (i in 1:k) {
  for (j in 1:k) {
    Np[i, j] <- integrate(Np_f, lower = -1, upper = 1, i = i, j = j, mu = mu)[[1]]
  }
}

Kp_u <- function(u, p, mu, Np) {
  K_u <- Ku(u, mu)
  Mp <- Np
  for (i in 0:p) {
    Mp[i + 1, 1] <- u^i
  }
  
  (det(Mp) / det(Np)) * K_u
  
}

RK_f <- function(u, p, mu, Mp, Np) {
  (Kp_u(u = u, p = p, mu = mu, Np = Np))^2
}

m <- 1000000
u_vals <- seq(-m, m) / (m + 0.5)
RKp_vals <- u_vals

l <- 2 * m + 1

for (i in 1:l) {
  RKp_vals[[i]] <- RK_f(u_vals[[i]], p = p, mu = mu, Np = Np)
}

RKp <- sum(RKp_vals) / m

RK <- sum(Ku(u_vals, mu = mu)^2) / m

K0 <- Kp_u(u = 0, p = p, mu = mu, Np = Np)

C <- RKp + (s - 1) * RK

CF <- round((2 * k * (2 * (K0 + (s - 1) * Ku(0, mu)) / C - 1))^(1 / (2 * k + 1)), 4)
CF

}

CFs <- vector(mode = "list", length = 2)

mat1 <- matrix(NA, nrow = 4, ncol = 2)
rownames(mat1) <- as.character(0:3)
colnames(mat1) <- as.character(c(1, 3))

names(CFs) <- c("4", "12")

CFs$`4` <- mat1
CFs$`12` <- mat1

for (p in c(1, 3)) {
  for (mu in 0:3) {
    for (s in c(4, 12)) {
      CFs[[as.character(s)]][as.character(mu), as.character(p)] <- CF_calc(p, mu, s)
    }
  }
}