setClass("deseats_fc",
  slots = c(
    pred = "ts",
    interv = "ts",
    obs = "ts",
    ts_name = "character"
  )
)


create_deseats_fc <- function(pred, interv, obs, ts_name) {
  
  methods::new("deseats_fc",
    pred = pred,
    interv = interv,
    obs = obs,
    ts_name = ts_name
  )
  
}

#'@export

setMethod("plot", "deseats_fc", function(x, ...) {
  
  dots <- list(...)
  defaults <- list(
    xlab = "Time",
    main = paste0('The last observations of "', x@ts_name, '" together with point and interval forecasts'),
    ylab = x@ts_name,
    xlim = c(c(time(x@obs))[[1]], utils::tail(c(time(x@pred)), 1)),
    ylim = c(min(x@obs, x@interv), max(x@obs, x@interv))
  )
  dots <- set_default(dots, defaults)
  dots[["type"]] = "n"
  dots[["x"]] <- 0
  dots[["y"]] <- 0
  do.call(plot, args = dots)
  m <- length(x@interv[1, ])
  n <- m / 2
  for (i in 1:n) {
    t <- c(time(x@interv))
    graphics::polygon(c(t, rev(t)), c(c(x@interv[, i]), rev(c(x@interv[, m - (i - 1)]))),
            border = NA, col = ggplot2::alpha("blue", 0.1))
    graphics::lines(c(time(x@obs)), c(x@obs), col = "black")
    graphics::lines(c(time(x@pred)), c(x@pred), col = "blue")
  }
  
})