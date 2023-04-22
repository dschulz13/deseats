ws_t <- deseats:::bv_mats$monthly$trend
ws_s <- deseats:::bv_mats$monthly$season

l_t <- length(ws_t[, 1])
l_s <- length(ws_s[, 1])

t_mid <- (l_t - 1) / 2 + 1

freq <- seq(0, 0.5, 0.0001)

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

deseason_filter_fun <- function(which_s = 1, ws_t = deseats:::bv_mats$monthly$trend, ws_s = deseats:::bv_mats$monthly$season) {

l_t_half <- (length(ws_t[, 1]) - 1) / 2
l_t_complete <- length(ws_t[1, ])

l_s_complete <- length(ws_s[1, ])

l <- l_s_complete + l_t_complete

ws_detrend <- matrix(0, ncol = l + l_t_half, nrow = l_s_complete)

# l_s <- l_t2
# 
# ll <- length(ws_t[1, ])

for (i in 1:l_t_half) {
  zero_v <- rep(0, l_t_complete)
  zero_v[[i]] <- 1
  ws_detrend[i, 1:l_t_complete] <- zero_v - ws_t[i, ]
}

t_mid <- l_t_half + 1

zero_v <- rep(0, l_t_complete)
zero_v[[t_mid]] <- 1
detrend_f_mid <- zero_v - ws_t[t_mid, ]
for (i in (t_mid:length(ws_s[which_s, ])) - l_t_half) {
  
  ws_detrend[i + l_t_half, i:(i + l_t_complete - 1)] <- detrend_f_mid
  
}

a <- apply(c(ws_s[which_s, ]) * ws_detrend, MARGIN = 2, FUN = sum)

l_a <- length(a)

zero_v <- rep(0, l_a)
zero_v[[which_s]] <- 1

v_s <- zero_v - a

freq <- seq(0, 0.5, 0.0001)
freq_pi <- seq(0, pi, length.out = length(freq))

gainF <- deseats::create.gain(v_s, zero.at = which_s)
out <- gainF(freq)
out

}

#

t1 <- deseason_filter_fun(which_s = 1)
t2 <- deseason_filter_fun(which_s = 2)
t3 <- deseason_filter_fun(which_s = 3)
t_mid <- deseason_filter_fun(which_s = (length(ws_s[, 1]) - 1) / 2 + 1)

freq <- seq(0, 0.5, 0.0001)
freq_pi <- seq(0, pi, length.out = length(freq))

matplot(freq_pi, cbind(t1, t2, t3, t_mid), type = "n", lty = 4:1, col = rep("black", 4),
        ylim = c(0, 1.4), xlim = c(-0.02, pi + 0.02), yaxs = "i", xaxs = "i", xaxt = "n",
        xlab = "Frequency", ylab = "Gain function value", yaxt = "n")
title(main = "Gain functions of selected BV4.1 deseasonalization filters for monthly time series")
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

lines(freq_pi, t1, lty = 4, col = "black")
lines(freq_pi, t2, lty = 3, col = "black")
lines(freq_pi, t3, lty = 2, col = "black")
lines(freq_pi, t_mid, lty = 1, col = "black", lwd = 2)

# legend("topright", 
#        legend = c(
#          "filter at first estimation time point",
#          "filter at second estimation time point",
#          "filter at third estimation time point",
#          "central filter"
#        ),
#        title = "Gain function of",
#        lty = 4:1,
#        lwd = c(1, 1, 1, 2),
#        col = rep("black", 4)
# )

#

t1 <- deseason_filter_fun(which_s = 1, ws_t = deseats:::bv_mats$quarterly$trend, ws_s = deseats:::bv_mats$quarterly$season)
t2 <- deseason_filter_fun(which_s = 2, ws_t = deseats:::bv_mats$quarterly$trend, ws_s = deseats:::bv_mats$quarterly$season)
t3 <- deseason_filter_fun(which_s = 3, ws_t = deseats:::bv_mats$quarterly$trend, ws_s = deseats:::bv_mats$quarterly$season)
t_mid <- deseason_filter_fun(which_s = (length(deseats:::bv_mats$quarterly$season[, 1]) - 1) / 2 + 1, ws_t = deseats:::bv_mats$quarterly$trend, ws_s = deseats:::bv_mats$quarterly$season)

freq <- seq(0, 0.5, 0.0001)
freq_pi <- seq(0, pi, length.out = length(freq))

matplot(freq_pi, cbind(t1, t2, t3, t_mid), type = "n", lty = 4:1, col = rep("black", 4),
        ylim = c(0, 1.3), xlim = c(-0.02, pi + 0.02), yaxs = "i", xaxs = "i", xaxt = "n",
        xlab = "Frequency", ylab = "Gain function value", yaxt = "n")
title(main = "Gain functions of selected BV4.1 deseasonalization estimation filters for quarterly time series")
title(sub = "Own recreation of a graphic in Speth (2004).", col.sub = "grey50", adj = 0)
axis(side = 1, at = seq(0, pi, pi / 2), 
     labels = expression(
       0,
       pi / 2,
       pi
     )
)
axis(side = 2, at = seq(0, 1.2, 0.1), 
     labels = sprintf("%.1f", seq(0, 1.2, 0.1))
)
abline(h = seq(0.1, 1.2, 0.1), col = "lightgray" , lty = 1)

lines(freq_pi, t1, lty = 4, col = "black")
lines(freq_pi, t2, lty = 3, col = "black")
lines(freq_pi, t3, lty = 2, col = "black")
lines(freq_pi, t_mid, lty = 1, col = "black", lwd = 2)

# legend("bottomleft", 
#        legend = c(
#          "filter at first estimation time point",
#          "filter at second estimation time point",
#          "filter at third estimation time point",
#          "central filter"
#        ),
#        title = "Gain function of",
#        lty = 4:1,
#        lwd = c(1, 1, 1, 2),
#        col = rep("black", 4)
# )

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