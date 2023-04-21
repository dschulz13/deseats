ws_t <- deseats:::bv_mats$monthly$trend
ws_s <- deseats:::bv_mats$monthly$season

l_t <- length(ws_t[, 1])
l_s <- length(ws_s[, 1])

t_mid <- (l_t - 1) / 2 + 1

freq <- seq(0, 0.5, 0.001)

gt_1 <- deseats::create.gain(ws_t[1, ], zero.at = 1)
gt_2 <- deseats::create.gain(ws_t[2, ], zero.at = 2)
gt_3 <- deseats::create.gain(ws_t[3, ], zero.at = 3)
gt_4 <- deseats::create.gain(ws_t[t_mid, ], zero.at = t_mid)

ot_1 <- gt_1(lambda = freq)
ot_2 <- gt_2(lambda = freq)
ot_3 <- gt_3(lambda = freq)
ot_4 <- gt_4(lambda = freq)

freq_pi <- seq(0, pi, length.out = length(freq))


matplot(freq_pi, cbind(ot_1, ot_2, ot_3, ot_4), type = "n", lty = 4:1, col = rep("black", 4),
        ylim = c(0, 1.4), xlim = c(-0.02, pi + 0.02), yaxs = "i", xaxs = "i", xaxt = "n",
        xlab = "Frequency", ylab = "Gain function value", yaxt = "n")
title(main = "Gain functions of selected BV4.1 trend-cycle estimation filters for monthly time series")
title(sub = "Own recreation of a graphic in Speth (2004).", col.sub = "grey50", adj = 0)
axis(side = 1, at = seq(0, pi, pi / 6), 
     labels = expression(
       0,
       pi / 6,
       2 * pi / 6,
       3 * pi / 6,
       4 * pi / 6,
       5 * pi / 6,
       pi
     )
)
axis(side = 2, at = seq(0, 1.4, 0.1), 
     labels = sprintf("%.1f", seq(0, 1.4, 0.1))
)
abline(h = seq(0.1, 1.3, 0.1), col = "lightgray" , lty = 1)

lines(freq_pi, ot_1, lty = 4, col = "black")
lines(freq_pi, ot_2, lty = 3, col = "black")
lines(freq_pi, ot_3, lty = 2, col = "black")
lines(freq_pi, ot_4, lty = 1, col = "black", lwd = 2)

legend("topright", 
       legend = c(
         "filter at first estimation time point",
         "filter at second estimation time point",
         "filter at third estimation time point",
         "central filter"
       ),
       title = "Gain function of",
       lty = 4:1,
       lwd = c(1, 1, 1, 2),
       col = rep("black", 4)
)

#==============================================================================#

l_t2 <- (length(ws_t[1,]) - 1) / 2

l_s <- length(ws_s[1, ])

l_s2 <- l_s + l_t2

ws_detrend <- matrix(0, ncol = l_s2, nrow = l_s)

l_s <- (l_t - 1) / 2

ll <- length(ws_t[1, ])

for (i in 1:(l_s)) {
  zero_v <- rep(0, ll)
  zero_v[[i]] <- 1
  ws_detrend[i, 1:ll] <- zero_v - ws_t[i, ]
}

zero_v <- rep(0, ll)
zero_v[[t_mid]] <- 1
detrend_f_mid <- zero_v - ws_t[t_mid, ]
for (i in ((l_s + 1):length(ws_s[1, ])) - l_s) {
  
  ws_detrend[i + l_s, i:(i + ll - 1)] <- detrend_f_mid
  
}
ws_detrend[t_mid, ] <- zero_v - ws_detrend[t_mid, ]

ws_detrend <- rbind(
  ws_detrend[1:l_s, ],
  matrix(rep(ws_detrend[t_mid, ], 100), nrow = 100, ncol = ll, byrow = TRUE),
  ws_detrend[(t_mid + 1):l_t, ] 
)

ws_test <- ws_detrend


l_ss <- length(ws_s[1, ])

l_dt <- length(ws_test[1, ])
ll_dt <- length(ws_test[, 1])

ws_test <- cbind(ws_test, matrix(0, ncol = l_ss - l_dt, nrow = ll_dt))

zero_v <- rep(0, l_ss)
zero_v[[1]] <- 1
ws_test2 <- apply(ws_test[1:l_ss, ] * ws_s[1, , drop = TRUE], MARGIN = 2, FUN = sum)
a <- zero_v - ws_test2
gainF <- deseats::create.gain(a, 1)
out <- gainF(freq)

#==============================================================================#

ws_t <- deseats:::bv_mats$quarterly$trend
ws_s <- deseats:::bv_mats$quarterly$season

l_t <- length(ws_t[, 1])
l_s <- length(ws_s[, 1])

t_mid <- (l_t - 1) / 2 + 1

freq <- seq(0, 0.5, 0.001)

gt_1 <- deseats::create.gain(ws_t[1, ], zero.at = 1)
gt_2 <- deseats::create.gain(ws_t[2, ], zero.at = 2)
gt_3 <- deseats::create.gain(ws_t[3, ], zero.at = 3)
gt_4 <- deseats::create.gain(ws_t[t_mid, ], zero.at = t_mid)

ot_1 <- gt_1(lambda = freq)
ot_2 <- gt_2(lambda = freq)
ot_3 <- gt_3(lambda = freq)
ot_4 <- gt_4(lambda = freq)

freq_pi <- seq(0, pi, length.out = length(freq))


matplot(freq_pi, cbind(ot_1, ot_2, ot_3, ot_4), type = "n", lty = 4:1, col = rep("black", 4),
        ylim = c(0, 1.4), xlim = c(-0.02, pi + 0.02), yaxs = "i", xaxs = "i", xaxt = "n",
        xlab = "Frequency", ylab = "Gain function value", yaxt = "n")
title(main = "Gain functions of selected BV4.1 trend-cycle estimation filters for quarterly time series")
title(sub = "Own recreation of a graphic in Speth (2004).", col.sub = "grey50", adj = 0)
axis(side = 1, at = seq(0, pi, pi / 2), 
     labels = expression(
       0,
       pi / 2,
       pi
     )
)
axis(side = 2, at = seq(0, 1.4, 0.1), 
     labels = sprintf("%.1f", seq(0, 1.4, 0.1))
)
abline(h = seq(0.1, 1.3, 0.1), col = "lightgray" , lty = 1)

lines(freq_pi, ot_1, lty = 4, col = "black")
lines(freq_pi, ot_2, lty = 3, col = "black")
lines(freq_pi, ot_3, lty = 2, col = "black")
lines(freq_pi, ot_4, lty = 1, col = "black", lwd = 2)

legend("topright", 
       legend = c(
         "filter at first estimation time point",
         "filter at second estimation time point",
         "filter at third estimation time point",
         "central filter"
       ),
       title = "Gain function of",
       lty = 4:1,
       lwd = c(1, 1, 1, 2),
       col = rep("black", 4)
)