
#'@export
animate_deseats <- function(object, ...) {
  n <- length(object@decomp[, 1])                  # Number of observations
  habs <- (length(object@weights[, 1, "Trend"]) - 1) / 2  # Number of boundary points 
  
  t <- c(time(object@decomp))
  
  oldPar <- par(no.readonly = TRUE)
  on.exit(par(oldPar), add = TRUE)
  d <- abs(diff(range(object@decomp[, 1])))
  min_y <- min(object@decomp[, 1])
  max_y <- max(object@decomp[, 1])  
  col_weights <- "#00D40E"
  col_fit <- "red"
  
  ani_opts_old <- animation::ani.options()
  on.exit(animation::ani.options(ani_opts_old), add = TRUE)
  animation::ani.options(nmax = 2 * (habs + 1), interval = 0.05, ani.width = 1200)  
    
  par(mar = c(5, 4, 4, 4) + 0.1)    
    
  for (frame in seq_len(habs + 1)) {
    plot(object@decomp[, 1], col = "grey 74",
         main = "Locally weighted regression illustrated", ylab = "")
    axis(side = 2, col = col_fit, col.ticks = col_fit, col.axis = col_fit)
    mtext("Estimated trend + seasonality", side = 2, line = 3, col = col_fit)      
    polygon(c(rep(t[[1]], 2), rep(t[[2 * habs + 1]], 2)), 
            c(min_y - 0.1 * d, max_y + 0.1 * d, max_y + 0.1 * d, min_y - 0.1 * d),
            border = NA, col = ggplot2::alpha("deepskyblue", 0.1))
    abline(v = c(t[[1]], t[[2 * habs + 1]]), col = "deepskyblue4", lwd = 2)
    abline(v = t[[frame]], col = "orange", lty = "dashed", lwd = 2)
    lines(t[1:frame], fitted(object)[1:frame], col = col_fit)
    points(t[[frame]], fitted(object)[[frame]], col = ggplot2::alpha("purple", 0.5), pch = 19, cex = 1.5)    
    
    par(new = TRUE)                             # Add new plot
    plot(0,                                     # Create second plot without axes
    axes = FALSE, xlab = "", ylab = "", type = "n", xlim = c(min(t), max(t)),
    ylim = c(min(object@weights[, , "Combined"]), max(object@weights[, , "Combined"])))
    abline(h = 0, col = "lightgray", lty = "dotted")
    lines(t[1:(2 * habs + 1)], object@weights[frame, , "Combined"], col = ggplot2::alpha(col_weights, 0.9))
    axis(side = 4, col = col_weights, col.ticks = col_weights,
         col.axis = col_weights)       # Add second axis
    mtext("Active kernel weights", side = 4, line = 3, col = col_weights) 
    
    animation::ani.pause()
  }
  
  for (frame in (n - habs):n) {
    plot(object@decomp[, 1], col = "grey 74",
         main = "Locally weighted regression illustrated", ylab = "")
    axis(side = 2, col = col_fit, col.ticks = col_fit, col.axis = col_fit)
    mtext("Estimated trend + seasonality", side = 2, line = 3, col = col_fit)      
    polygon(c(rep(t[[n - 2 * habs]], 2), rep(t[[n]], 2)), 
            c(min_y - 0.1 * d, max_y + 0.1 * d, max_y + 0.1 * d, min_y - 0.1 * d),
            border = NA, col = ggplot2::alpha("deepskyblue", 0.1))
    abline(v = c(t[[n - 2 * habs]], t[[n]]), col = "deepskyblue4", lwd = 2)
    abline(v = t[[frame]], col = "orange", lty = "dashed", lwd = 2)
    lines(t[1:frame], fitted(object)[1:frame], col = col_fit)
    points(t[[frame]], fitted(object)[[frame]], col = ggplot2::alpha("purple", 0.5), pch = 19, cex = 1.5)
    
    par(new = TRUE)                             # Add new plot
    plot(0,                                     # Create second plot without axes
    axes = FALSE, xlab = "", ylab = "", type = "n", xlim = c(min(t), max(t)),
    ylim = c(min(object@weights[, , "Combined"]), max(object@weights[, , "Combined"])))
    abline(h = 0, col = "lightgray", lty = "dotted")
    
    lines(t[(n - 2 * habs):n], object@weights[frame + 2 * habs - n + 1, , "Combined"], col = ggplot2::alpha(col_weights, 0.9))
    axis(side = 4, col = col_weights, col.ticks = col_weights,
         col.axis = col_weights)       # Add second axis
    mtext("Active kernel weights", side = 4, line = 3, col = col_weights) 
    animation::ani.pause()
  }
  
}