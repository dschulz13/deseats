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

df_t_monthly <- data.frame(
  freq = freq_pi,
  t1 = ot_1,
  t2 = ot_2,
  t3 = ot_3,
  central = ot_4
)

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

deseason_central_filter_gain <- function(which = c("quarterly", "monthly"), lambda = seq(0, 0.5, 0.0001)) {
  
  which <- match.arg(which)
  
  if (which == "quarterly") {
    
    w_t <- deseats:::bv_mats$quarterly$trend
    w_s <- deseats:::bv_mats$quarterly$season
    
  } else if (which == "monthly") {
    
    w_t <- deseats:::bv_mats$monthly$trend
    w_s <- deseats:::bv_mats$monthly$season    
    
  }
  
  l_t <- length(w_t[, 1])
  l_s <- length(w_s[, 1])
  
  pos_t_mid <- (l_t - 1) / 2 + 1
  pos_s_mid <- (l_s - 1) / 2 + 1
  
  t_filt <- w_t[pos_t_mid, ]
  t_filt <- t_filt[t_filt != 0]
  l_t_filt <- length(t_filt)
  
  s_filt <- w_s[pos_s_mid, ]
  s_filt <- s_filt[s_filt != 0]  
  l_s_filt <- length(s_filt)
  
  
  zero_v <- rep(0, l_t_filt)
  pos_t <- (l_t_filt - 1) / 2 + 1
  zero_v[[pos_t]] <- 1
  
  dt_filt <- zero_v - t_filt
  
  mat_detrend <- matrix(0, nrow = l_s_filt, ncol = l_t_filt + (l_s_filt - 1))
  
  for (i in 1:l_s_filt) {
    
    mat_detrend[i, i:(i + l_t_filt - 1)] <- dt_filt
    
  }
  
  s_filt <- c(apply(
    c(s_filt) * mat_detrend, MARGIN = 2, FUN = sum
  ))
  l_s_filt <- length(s_filt)
  
  pos_s <- (l_s_filt - 1) / 2 + 1
  
  zero_v <- rep(0, l_s_filt)
  zero_v[[pos_s]] <- 1
  
  ds_filt <- zero_v - s_filt
  
  gF <- deseats::create.gain(ds_filt, zero.at = pos_s)
  
  gain_v <- gF(lambda)
  gain_v
  
}

#

t1 <- deseason_filter_fun(which_s = 1)
t2 <- deseason_filter_fun(which_s = 2)
t3 <- deseason_filter_fun(which_s = 3)
t_mid <- deseason_central_filter_gain("monthly")

freq <- seq(0, 0.5, 0.0001)
freq_pi <- seq(0, pi, length.out = length(freq))

df_ds_monthly <- data.frame(
  freq = freq_pi,
  t1 = t1,
  t2 = t2,
  t3 = t3,
  central = t_mid
)

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
       col = rep("black", 4),
       cex = 0.9
)

#

t1 <- deseason_filter_fun(which_s = 1, ws_t = deseats:::bv_mats$quarterly$trend, ws_s = deseats:::bv_mats$quarterly$season)
t2 <- deseason_filter_fun(which_s = 2, ws_t = deseats:::bv_mats$quarterly$trend, ws_s = deseats:::bv_mats$quarterly$season)
t3 <- deseason_filter_fun(which_s = 3, ws_t = deseats:::bv_mats$quarterly$trend, ws_s = deseats:::bv_mats$quarterly$season)
t_mid <- deseason_central_filter_gain("quarterly")

freq <- seq(0, 0.5, 0.0001)
freq_pi <- seq(0, pi, length.out = length(freq))

df_ds_quarterly <- data.frame(
  freq = freq_pi,
  t1 = t1,
  t2 = t2,
  t3 = t3,
  central = t_mid
)

matplot(freq_pi, cbind(t1, t2, t3, t_mid), type = "n", lty = 4:1, col = rep("black", 4),
        ylim = c(0, 1.3), xlim = c(-0.02, pi + 0.02), yaxs = "i", xaxs = "i", xaxt = "n",
        xlab = "Frequency", ylab = "Gain function value", yaxt = "n")
title(main = "Gain functions of selected BV4.1 deseasonalization filters for quarterly time series")
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

legend("bottomleft",
       legend = c(
         "filter at first estimation time point",
         "filter at second estimation time point",
         "filter at third estimation time point",
         "central filter"
       ),
       title = "Gain function of",
       lty = 4:1,
       lwd = c(1, 1, 1, 2),
       col = rep("black", 4),
       cex = 0.8
)

#==============================================================================#

ws_t <- deseats:::bv_mats$quarterly$trend
ws_s <- deseats:::bv_mats$quarterly$season

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

df_t_quarterly <- data.frame(
  freq = freq_pi,
  t1 = ot_1,
  t2 = ot_2,
  t3 = ot_3,
  central = ot_4
)

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

#==========================================================

# Extensive plot

df_all <- data.frame(
  Frequency = df_ds_monthly$freq,
  "trend_t1_quarterly" = df_t_quarterly$t1
)
df_all[["trend_t2_quarterly"]] = df_t_quarterly$t2
df_all[["trend_t3_quarterly"]] = df_t_quarterly$t3
df_all[["trend_central_quarterly"]] = df_t_quarterly$central

df_all[["trend_t1_monthly"]] = df_t_monthly$t1
df_all[["trend_t2_monthly"]] = df_t_monthly$t2
df_all[["trend_t3_monthly"]] = df_t_monthly$t3
df_all[["trend_central_monthly"]] = df_t_monthly$central

df_all[["deseason_t1_quarterly"]] = df_ds_quarterly$t1
df_all[["deseason_t2_quarterly"]] = df_ds_quarterly$t2
df_all[["deseason_t3_quarterly"]] = df_ds_quarterly$t3
df_all[["deseason_central_quarterly"]] = df_ds_quarterly$central

df_all[["deseason_t1_monthly"]] = df_ds_monthly$t1
df_all[["deseason_t2_monthly"]] = df_ds_monthly$t2
df_all[["deseason_t3_monthly"]] = df_ds_monthly$t3
df_all[["deseason_central_monthly"]] = df_ds_monthly$central

library(tidyverse)
library(ggpubr)

df_all2 <- pivot_longer(
  df_all, 2:17, names_to = c("Method", "TimePoint", "Type"), names_sep = "_", values_to = "Gain"
)

df_all2$Method <- ifelse(df_all2$Method == "trend", "Trend filter", "Seasonal adjustment filter")
df_all2$TimePoint[df_all2$TimePoint == "t1"] <- "First time point"
df_all2$TimePoint[df_all2$TimePoint == "t2"] <- "Second time point"
df_all2$TimePoint[df_all2$TimePoint == "t3"] <- "Third time point"
df_all2$TimePoint[df_all2$TimePoint == "central"] <- "Central"

df_all2$Type[df_all2$Type == "quarterly"] <- "Quarterly time series"
df_all2$Type[df_all2$Type == "monthly"] <- "Monthly time series"

names(df_all2) <- c("Frequency", "Method", "Filter", "Type", "Gain")
lwd <- c("First time point" = 0.5, "Second time point" = 0.5, "Third time point" = 0.5, "Central" = 0.8)

plot_df <- ggplot(df_all2, aes(x = Frequency, y = Gain)) +
  geom_line(aes(linetype = Filter, linewidth = Filter)) +
  scale_linewidth_manual(name = "Filter", values = lwd) + 
  scale_linetype_manual(name = "Filter", values = 1:4) +
  facet_grid(factor(Method, levels = c("Trend filter", "Seasonal adjustment filter")) ~ factor(Type, levels = c("Quarterly time series", "Monthly time series"))) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 0, color = "grey50")
  ) +
  ylab("Gain function") +
  ggtitle("Gain functions of selected BV4.1 filters") + 
  scale_x_continuous(
    breaks = seq(0, pi, 1 / 6 * pi), 
    labels = expression(0, pi / 6, 2 * pi / 6, 3 * pi / 6, 4 * pi / 6, 5 * pi / 6, pi),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1.4, 0.1),
    minor_breaks = NULL,
    expand = c(0, 0)
  ) +
  labs(caption = "Own recreation of graphics in Speth (2004).") +
  coord_cartesian(xlim = c(0 - 0.02, pi + 0.02), ylim = c(0 - 0.02, 1.4 + 0.02))


plot_df

bv_gain <- df_all2

# usethis::use_data(bv_gain, internal = TRUE, overwrite = TRUE)

ggsave(
  filename = "BV4.1 Gain Functions.png",
  plot = plot_df,
  units = "cm",
  width = 35, height = 18,
  dpi = 1200
)