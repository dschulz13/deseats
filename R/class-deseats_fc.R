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

setMethod("expo", "deseats_fc", function(object) {
  
  object@pred <- exp(object@pred)
  object@interv <- exp(object@interv)
  object@obs <- exp(object@obs)
  object@ts_name <- paste0("exp(", object@ts_name, ")")
  object
  
})

#'@export

setMethod("plot", "deseats_fc", function(x, ...) {
  
  dots <- list(...)
  defaults <- list(
    xlab = "Time",
    main = paste0('The observations of "', x@ts_name, '" together with point and interval forecasts'),
    ylab = x@ts_name,
    xlim = c(c(time(x@obs))[[1]], utils::tail(c(time(x@pred)), 1)),
    ylim = c(min(x@obs, x@interv), max(x@obs, x@interv))
  )
  dots <- set_default(dots, defaults)
  if (!is.null(dots[["col"]])) {
    col <- dots$col
    col1 <- col[[1]]
    col2 <- col[[2]]
    dots[["col"]] <- NULL
  } else {
    col1 <- "black"
    col2 <- "blue"
  } 
  dots[["type"]] = "n"
  dots[["x"]] <- 0
  dots[["y"]] <- 0
  do.call(plot, args = dots)
  m <- length(x@interv[1, ])
  n <- m / 2
  for (i in 1:n) {
    t <- c(time(x@interv))
    graphics::polygon(c(t, rev(t)), c(c(x@interv[, i]), rev(c(x@interv[, m - (i - 1)]))),
            border = NA, col = ggplot2::alpha(col2, 0.1 + (i - 1) * 0.05))
    graphics::lines(c(time(x@obs)), c(x@obs), col = col1)
    graphics::lines(c(time(x@pred)), c(x@pred), col = col2)
  }
  
})

#'@export

setMethod("autoplot", "deseats_fc", function(object, ...) {
  
  .df1 <- data.frame(
    Time = c(time(object@obs)),
    Observations = c(object@obs),
    Color = "1"
  )
  .df2 <- data.frame(
    Time = c(time(object@pred)),
    Forecasts = c(object@pred),
    Color = "2"
  )
  .df3 <- as.data.frame(object@interv)
  
  cnames <- colnames(object@interv)
  n <- length(object@interv[1, ])
  m <- n / 2  
  
  .df4 <- data.frame(
    ymin = c(unname(unlist(.df3[, 1:m]))),
    ymax = c(unname(unlist(.df3[, n:(n - m + 1)]))),
    Time = rep(c(time(object@pred)), m),
    Case = rep(as.character(1:m), each = length(object@pred))
  )

  p_out <- ggplot2::ggplot(.df1) +
    ggplot2::geom_line(ggplot2::aes_string(x = "Time", y = "Observations", color = "Color"))
  
    p_out <- p_out +
      ggplot2::geom_ribbon(data = .df4, ggplot2::aes_string(x = "Time", ymin = "ymin", ymax = "ymax",
                                                            fill = "Case"),
                           inherit.aes = FALSE, outline.type = NULL)
  
  fill_color = ggplot2::alpha("blue", seq(0.1, 0.1 + (m - 1) * 0.05, 0.05))  
    
  p_out <- p_out +
    ggplot2::geom_line(data = .df2, ggplot2::aes_string(x = "Time", y = "Forecasts", color = "Color"),
                       inherit.aes = FALSE) +
    ggplot2::xlab("Time") +
    ggplot2::ylab(object@ts_name) +
    ggplot2::ggtitle(paste0('The observations of "', object@ts_name, '" together with point and interval forecasts')) +
    ggplot2::scale_fill_manual(name = "Intervals", values = fill_color, 
                               labels = paste0((1 - 2 * (as.numeric(substr(cnames[1:m], 1, nchar(cnames[1:m]) - 1)) / 100)) * 100, "%")) +
    ggplot2::scale_color_manual(name = "Series", values = c("black", "blue"), 
                                labels = c("Observations", "Forecasts"))
  
  p_out
  
})
