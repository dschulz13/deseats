aniSave <- function(fun) {
  switch(
    fun,
    HTML = animation::saveHTML,
    Latex = animation::saveLatex,
    GIF = animation::saveGIF
  )
}

#'@export
methods::setGeneric("animate", function(object, ...) standardGeneric("animate"))
#'@export
methods::setMethod("animate", "deseats", 
function(object, col.obs = "grey74", col.fit = "red", 
                    col.weights = "#00D40E", col.window = "deepskyblue4", 
                    col.spot = "orange", save = NULL,
                    xlab = "Time", ylab1 = "Estimated trend + seasonality",
                    ylab2 = "Active kernel weights",
                    main = NULL, ...) {
  
  n <- length(object@decomp[, 1])                  # Number of observations
  habs <- (length(object@weights[, 1, "Trend"]) - 1) / 2  # Number of boundary points 
  
  t <- c(time(object@decomp))
  tsub <- 1:n
  
  oldPar <- par(no.readonly = TRUE)
  on.exit(par(oldPar), add = TRUE)
  
  parNew <- list(
    mar = c(5, 4, 4, 4) + 0.1
  )

  d <- abs(diff(range(object@decomp[, 1])))
  min_y <- min(object@decomp[, 1])
  max_y <- max(object@decomp[, 1])  
  
  mat_reg <- as.data.frame(matrix(0, ncol = object@order_poly + object@frequency,
                           nrow = 2 * habs + 1))
  names(mat_reg) <- c("y", paste0("t", 1:object@order_poly), 
                      paste0("cos", 1:trunc((object@frequency - 1) / 2)),
                      paste0("sin", 1:trunc((object@frequency - 1) / 2)),
                      paste0("cos", (trunc(object@frequency - 1) / 2) + 1))
  mat_reg[["y"]] <- object@decomp[1:(2 * habs + 1), 1]
  
  ani_opts_old <- animation::ani.options()
  on.exit(animation::ani.options(ani_opts_old), add = TRUE)
  animation::ani.options(nmax = 2 * (habs + 1), interval = 0.1, ani.width = 1200)  
    
  if (is.null(main)) {
    main <- paste0('Locally weighted regression illustrated for the series "', 
                       object@ts_name, '"')
  }
  
  .f <- function() {
  
  par(parNew)    
    
  for (frame in seq_len(habs + 1)) {
    
    tt <- (tsub[1:(2 * habs + 1)] - tsub[[frame]]) / habs
    for (i in 1:object@order_poly) {
      mat_reg[[paste0("t", i)]] <- tt^i
    }
    for (i in 1:((trunc(object@frequency - 1) / 2))) {
      mat_reg[[paste0("cos", i)]] <- cos(2 * i * pi * tt / (object@frequency / habs))
      mat_reg[[paste0("sin", i)]] <- sin(2 * i * pi * tt / (object@frequency / habs))      
    }
    mat_reg[[paste0("cos", trunc(object@frequency - 1) / 2 + 1)]] <- cos(pi * tt * habs)
    
    lm_fit <- (stats::lm(y ~ ., data = mat_reg, weights = object@weights_wfun[frame, ]))$fitted.values
    
    plot(object@decomp[, 1], col = col.obs,
         main = main, ylab = "", xlab = xlab)
    axis(side = 2, col = col.fit, col.ticks = col.fit, col.axis = col.fit)
    mtext(ylab1, side = 2, line = 3, col = col.fit)      
    polygon(c(rep(t[[1]], 2), rep(t[[2 * habs + 1]], 2)), 
            c(min_y - 0.1 * d, max_y + 0.1 * d, max_y + 0.1 * d, min_y - 0.1 * d),
            border = NA, col = ggplot2::alpha(col.window, 0.1))
    abline(v = c(t[[1]], t[[2 * habs + 1]]), col = col.window, lwd = 2)
    abline(v = t[[frame]], col = col.spot, lty = "dashed", lwd = 2)
    lines(t[1:frame], fitted(object)[1:frame], col = col.fit, lwd = 1)    
    lines(t[1:(2 * habs + 1)], lm_fit, col = ggplot2::alpha("forestgreen", 1), lwd = 1, lty = 2)
    points(t[[frame]], fitted(object)[[frame]], col = ggplot2::alpha("purple", 0.5), pch = 19, cex = 1.5)    
    
    par(new = TRUE)                             # Add new plot
    plot(0,                                     # Create second plot without axes
    axes = FALSE, xlab = "", ylab = "", type = "n", xlim = c(min(t), max(t)),
    ylim = c(min(object@weights[, , "Combined"]), max(object@weights[, , "Combined"])))
    abline(h = 0, col = "lightgray", lty = "dotted")
    lines(t[1:(2 * habs + 1)], object@weights[frame, , "Combined"], col = ggplot2::alpha(col.weights, 0.9))
    axis(side = 4, col = col.weights, col.ticks = col.weights,
         col.axis = col.weights)       # Add second axis
    mtext(ylab2, side = 4, line = 3, col = col.weights) 
    
    animation::ani.pause()
  }
  
  mat_reg[["y"]] <- object@decomp[(n - 2 * habs):n, 1]
    
  for (frame in (n - habs):n) {
    
    tt <- (tsub[(n - 2 * habs):n] - tsub[[frame]]) / habs
    for (i in 1:object@order_poly) {
      mat_reg[[paste0("t", i)]] <- tt^i
    }
    for (i in 1:((trunc(object@frequency - 1) / 2))) {
      mat_reg[[paste0("cos", i)]] <- cos(2 * i * pi * tt / (object@frequency / habs))
      mat_reg[[paste0("sin", i)]] <- sin(2 * i * pi * tt / (object@frequency / habs))      
    }
    mat_reg[[paste0("cos", trunc(object@frequency - 1) / 2 + 1)]] <- cos(pi * tt * habs)
    
    lm_fit <- (stats::lm(y ~ ., data = mat_reg, weights = object@weights_wfun[frame + 2 * habs - n + 1, ]))$fitted.values
    
    plot(object@decomp[, 1], col = col.obs,
         main = main, 
         ylab = "", xlab = xlab)
    axis(side = 2, col = col.fit, col.ticks = col.fit, col.axis = col.fit)
    mtext(ylab1, side = 2, line = 3, col = col.fit)      
    polygon(c(rep(t[[n - 2 * habs]], 2), rep(t[[n]], 2)), 
            c(min_y - 0.1 * d, max_y + 0.1 * d, max_y + 0.1 * d, min_y - 0.1 * d),
            border = NA, col = ggplot2::alpha(col.window, 0.1))
    abline(v = c(t[[n - 2 * habs]], t[[n]]), col = col.window, lwd = 2)
    abline(v = t[[frame]], col = col.spot, lty = "dashed", lwd = 2)
    lines(t[1:frame], fitted(object)[1:frame], col = col.fit, lwd = 1)
    lines(t[(n - 2 * habs):n], lm_fit, col = ggplot2::alpha("forestgreen", 1), lwd = 1, lty = 2)    
    points(t[[frame]], fitted(object)[[frame]], col = ggplot2::alpha("purple", 0.5), pch = 19, cex = 1.5)
    
    par(new = TRUE)                             # Add new plot
    plot(0,                                     # Create second plot without axes
    axes = FALSE, xlab = "", ylab = "", type = "n", xlim = c(min(t), max(t)),
    ylim = c(min(object@weights[, , "Combined"]), max(object@weights[, , "Combined"])))
    abline(h = 0, col = "lightgray", lty = "dotted")
    
    lines(t[(n - 2 * habs):n], object@weights[frame + 2 * habs - n + 1, , "Combined"], col = ggplot2::alpha(col.weights, 0.9))
    axis(side = 4, col = col.weights, col.ticks = col.weights,
         col.axis = col.weights)       # Add second axis
    mtext(ylab2, side = 4, line = 3, col = col.weights) 
    animation::ani.pause()
  }
  
  }

if (is.null(save)) {
  .f()
} else {
  .savefun <- aniSave(save)
  .savefun({.f()}, interval = 0.1, ani.width = 1200)
}
}
)