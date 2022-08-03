# Plot subfunctions

plot_facets <- function(obj, ...) {
  dots <- list(...)
  defaults <- list(
    xlab = "Time",
    main = paste0('Decomposition of the time series "', obj@ts_name,'"')
  )
  dots <- set_default(dots, defaults)
  dots[["x"]] <- tryCatch(obj@decomp[, 1:4],
    error = function(c1) {
      obj@decomp[, 1:3]
    })
  
  do.call(stats::plot.ts, args = dots)
}

plot_resid <- function(obj, ...) {
  dots <- list(...)
  defaults <- list(
    xlab = "Time",
    main = paste0('The estimated rest in the time series "', obj@ts_name, '"'),
    ylab = "Residual value"
  )
  dots <- set_default(dots, defaults)
  dots[["x"]] <- residuals(obj)
  
  do.call(stats::plot.ts, args = dots)  
}

plot_fit <- function(obj, ...) {
  dots <- list(...)
  defaults <- list(
    xlab = "Time",
    main = paste0('The time series "', obj@ts_name, 
                  '" together with estimated trend and seasonality'),
    ylab = obj@ts_name,
    col = c("grey74", "red"),
    lty = c(1, 1),
    type = "ll"
  )
  dots <- set_default(dots, defaults)
  dots[["x"]] <- c(stats::time(obj@decomp))
  dots[["y"]] <- cbind(obj@decomp[, 1], fitted(obj))
  
  do.call(graphics::matplot, args = dots)  
}

plot_obs <- function(obj, ...) {
  dots <- list(...)
  defaults <- list(
    xlab = "Time",
    main = paste0('The observed time series "', obj@ts_name, '"'),
    ylab = obj@ts_name
  )
  dots <- set_default(dots, defaults)
  dots[["x"]] <- obj@decomp[, 1]
  
  do.call(stats::plot.ts, args = dots)  
}

plot_deseason <- function(obj, ...) {
  dots <- list(...)
  defaults <- list(
    xlab = "Time",
    main = paste0('The deseasonalized version of "', obj@ts_name, '"'),
    ylab = "Seasonally adjusted value"
  )
  dots <- set_default(dots, defaults)
  dots[["x"]] <- deseasonalize(obj)
  
  do.call(stats::plot.ts, args = dots)  
}

plot_detrend <- function(obj, ...) {
  dots <- list(...)
  defaults <- list(
    xlab = "Time",
    main = paste0('The detrended version of "', obj@ts_name, '"'),
    ylab = "Detrended value"
  )
  dots <- set_default(dots, defaults)
  dots[["x"]] <- detrend(obj)
  
  do.call(stats::plot.ts, args = dots)  
}

# Plot subfunctions ggplot2

plot_facets_gg <- function(obj, ...) {
  
    c_names <- colnames(obj@decomp)[1:4]
    c_names <- c_names[!is.na(c_names)]

    n <- length(obj@decomp[, 1])
    t <- c(stats::time(obj@decomp))

    df <- tryCatch({
      data.frame(
        Time = rep(t, 4),
        Value = c(as.numeric(obj@decomp[, 1:4])),
        Series = factor(c(rep(1:4, each = n)), labels = c_names)
      )      
    }, error = function(c1){
      data.frame(
        Time = rep(t, 3),
        Value = c(as.numeric(obj@decomp[, 1:3])),
        Series = factor(c(rep(1:3, each = n)), labels = c_names)
      )       
    })

    ggplot2::ggplot(df, ggplot2::aes_string(x = "Time", y = "Value")) +
      ggplot2::geom_line() +
      ggplot2::ggtitle(paste0('Decomposition of the time series "', obj@ts_name, '"')) +
      ggplot2::xlab("Time") +
      ggplot2::facet_grid(Series ~ ., scales = "free_y") +
      ggplot2::theme(axis.title.y = ggplot2::element_blank())

}

plot_resid_gg <- function(obj, ...) {
  
  res <- residuals(obj)
  
  df <- data.frame(
    Time = c(stats::time(res)),
    Value = c(res)
  )

  ggplot2::ggplot(df, ggplot2::aes_string(x = "Time", y = "Value")) +
    ggplot2::geom_line() +
    ggplot2::ylab("Residual value") +
    ggplot2::ggtitle(paste0('The estimated rest in the time series "', obj@ts_name, '"')) 
}

plot_fit_gg <- function(obj, ...) {

    n <- length(obj@decomp[, 1])
    t <- c(stats::time(obj@decomp))
    obs <- as.numeric(obj@decomp[, 1])
    fit <- as.numeric(fitted(obj))
    lo <- length(obs)
    lf <- length(fit)
    if (lo > lf) {
      fit <- c(rep(NA, lo - lf), fit)
    }

    df <- data.frame(
      Time = rep(t, 2),
      Value = c(obs, fit),
      Series = factor(c(rep(1:2, each = n)), labels = c("Observations", "Fitted values"))
    )

    p1 <- ggplot2::ggplot(df, ggplot2::aes_string(x = "Time", y = "Value")) +
      ggplot2::geom_line(ggplot2::aes_string(color = "Series")) +
      ggplot2::ggtitle(paste0('The time series "', obj@ts_name,
                  '" together with estimated trend and seasonality')) +
      ggplot2::ylab(obj@ts_name) +
        ggplot2::scale_color_manual(values = c("grey74", "red"))
    
    suppressWarnings(print(p1))

}

plot_obs_gg <- function(obj, ...) {
  df <- data.frame(
    Time = c(stats::time(obj@decomp)),
    Value = c(obj@decomp[, c(1)])
  )

  ggplot2::ggplot(df, ggplot2::aes_string(x = "Time", y = "Value")) + 
    ggplot2::geom_line() +
    ggplot2::ylab(obj@ts_name) +
    ggplot2::ggtitle(paste0('The observed time series "', obj@ts_name, '"')) 
}

plot_deseason_gg <- function(obj, ...) {
  df <- data.frame(
    Time = c(stats::time(obj@decomp)),
    Value = c(deseasonalize(obj))
  )

  ggplot2::ggplot(df, ggplot2::aes_string(x = "Time", y = "Value")) +
    ggplot2::geom_line() +
    ggplot2::ylab("Seasonally adjusted value") +
    ggplot2::ggtitle(paste0('The deseasonalized version of "', obj@ts_name, '"'))   
}

plot_detrend_gg <- function(obj, ...) {
  df <- data.frame(
    Time = c(stats::time(obj@decomp)),
    Value = c(detrend(obj))
  )

  ggplot2::ggplot(df, ggplot2::aes_string(x = "Time", y = "Value")) +
    ggplot2::geom_line() +
    ggplot2::ylab("Detrended value") +
    ggplot2::ggtitle(paste0('The detrended version of "', obj@ts_name, '"'))   
}