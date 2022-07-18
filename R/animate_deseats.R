animate_deseats <- function(object, ...) {
  n <- length(object@decomp[, 1])                  # Number of observations
  habs <- (length(object@weights[, 1, "Trend"]) - 1) / 2  # Number of boundary points 
  
  t <- c(time(object@decomp))
  
  oldPar <- par(no.readonly = TRUE)
  on.exit(par(oldPar), add = TRUE)
  par(mar = c(5, 4, 4, 4) + 0.1)
  
  for (frame in seq_len(habs + 1)) {
    plot(object@decomp[, 1], col = "grey 74")
    abline(v = c(t[[1]], t[[2 * habs + 1]]), col = "blue")
    abline(v = t[[frame]], col = "orange", lty = "dashed", lwd = 2)
    lines(t[1:frame], fitted(object)[1:frame], col = "red")
    
    par(new = TRUE)                             # Add new plot
    plot(0,                                     # Create second plot without axes
    axes = FALSE, xlab = "", ylab = "", type = "n", xlim = c(min(t), max(t)),
    ylim = c(min(object@weights[, , "Combined"]), max(object@weights[, , "Combined"])))
    abline(h = 0, col = "lightgray", lty = "dotted")
    lines(t[1:(2 * habs + 1)], object@weights[frame, , "Combined"], col = "forestgreen")
    axis(side = 4)      # Add second axis
    mtext("Active kernel weights", side = 4, line = 3)   
  }
  
}