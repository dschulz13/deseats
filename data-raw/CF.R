# p <- 3
# 
# 
# mu <- 2
# 
# s <- 4
  
CF_calc <- function(p, mu, s) {

k <- p + 1  
  
Ku <- function(u, mu) {
  
  if (mu >= 0 & mu <= 3) {
  
    mu_c <- as.character(mu)
    const <- switch(
      mu_c,
      "0" = 0.5,
      "1" = 0.75,
      "2" = 15 / 16,
      "3" = 35 / 32
    )
    out <- const * (1 - u^2)^mu
  
  }
  
  out
  
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

m <- 1000000
u_vals <- seq(-m, m) / (m + 0.5)
Kp_vals <- u_vals

l <- 2 * m + 1

for (i in 1:l) {
  Kp_vals[[i]] <- Kp_u(u_vals[[i]], p = p, mu = mu, Np = Np)
}

Kp_vals <- Kp_vals / sum(Kp_vals)
RKp <- sum(Kp_vals^2) * m

Ku_v <- Ku(u_vals, mu = mu)
Ku_v <- Ku_v / sum(Ku_v)
RK <- sum(Ku_v^2) * m

K0 <- Kp_vals[[m + 1]] * m
Ks0 <- Ku_v[[m + 1]] * m

C <- RKp + (s - 1) * RK


out <- list(
  CF = round((2 * k * (2 * (K0 + (s - 1) * Ks0) / C - 1))^(1 / (2 * k + 1)), 4),
  K_p_mu_0 = round(K0, 4),
  K_0 = round(Ku(0, mu), 4),
  RKp = round(RKp, 4),
  RK = round(RK, 4),
  C1 = round(K0 + (s - 1) * Ku(0, mu), 4),
  C2 = round(C, 4),
  C3 = round((2 * (K0 + (s - 1) * Ks0) / C - 1), 4)
)

out

}

CFs <- vector(mode = "list", length = 4)

mat1 <- matrix(NA, nrow = 4, ncol = 2)
rownames(mat1) <- as.character(0:3)
colnames(mat1) <- as.character(c(1, 3))

names(CFs) <- c("1", "4", "7", "12")

CFs$`1` <- mat1
CFs$`4` <- mat1
CFs$`7` <- mat1
CFs$`12` <- mat1

s_all <- c(1, 4, 7, 12)
mu_all <- 0:3

p_all <- c(1, 3)

df <- expand.grid(s_all, mu_all, p_all)
names(df) <- c("s", "mu", "p")
df <- df[order(df$s), ]
rownames(df) <- 1:length(df[, 1])

df$K_p_mu_0 <- 0
df$K_0 <- 0
df$RKp <- 0
df$RK <- 0
df$C1 <- 0
df$C2 <- 0
df$C3 <- 0
df$CF <- 0

for (p in p_all) {
  for (mu in mu_all) {
    for (s in s_all) {
      result <- CF_calc(p, mu, s)
      df[df$s == s & df$mu == mu & df$p == p, "CF"] <- result$CF
      df[df$s == s & df$mu == mu & df$p == p, "K_p_mu_0"] <- result$K_p_mu_0
      df[df$s == s & df$mu == mu & df$p == p, "K_0"] <- result$K_0      
      df[df$s == s & df$mu == mu & df$p == p, "RKp"] <- result$RKp
      df[df$s == s & df$mu == mu & df$p == p, "RK"] <- result$RK
      df[df$s == s & df$mu == mu & df$p == p, "C1"] <- result$C1
      df[df$s == s & df$mu == mu & df$p == p, "C2"] <- result$C2
      df[df$s == s & df$mu == mu & df$p == p, "C3"] <- result$C3       
    }
  }
}

CF_table <- df

# save(CF_table, file = "DeSeaTS_CF_Table.RData")
# write.table(
#   CF_table,
#   file = "DeSeaTS_CF_Table.csv",
#   quote = FALSE, sep = ",",
#   row.names = FALSE, col.names = TRUE
# )