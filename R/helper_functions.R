mu_select <- function(kernel_fun) {
  switch(kernel_fun,
    "uniform" = 0,
    "epanechnikov" = 1,
    "bisquare" = 2,
    "triweight" = 3
  )
}

bf_select <- function(boundary_method) {
  switch(boundary_method,
    "extend" = 1,
    "shorten" = 0
  )
}

set_default <- function(obj, repl) {
  arg_names <- names(repl)
  for (i in seq_along(arg_names)) {
    if (is.null(obj[[arg_names[[i]]]])) {
      obj[[arg_names[[i]]]] <- repl[[i]]
    }
  }
  obj
}

check_which <-function(which) {
  
    if (is.null(which)) {
    text_prompt <- data.frame(
      c("facets:", "observations:", "fitted:", "detailed_fit:","residuals:",
        "deseasonalized:", "detrended:"),
      c(
        "Facet plot of the estimated components",
        "The observed time series",
        "The observations together with estimated trend + seasonality",
        "The observations together with the fitted values and with the estimated trend",
        "The both detrended and seasonally adjusted series",
        "The seasonally adjusted series",
        "The detrended series"
      )
    )
    colnames(text_prompt) <- NULL
    
    cat("\nSelect one of the following plots via the keyword or the position number (exit with 0):\n")
    
    print.data.frame(text_prompt, row.names = FALSE, right = FALSE)
    
    cat("\n")
    
    which <- ""
    
    while(!(which %in% c(0, 1, 2, 3, 4, 5, 6, "facets", "observations", "fitted",
                         "detailed_fit", "residuals", "deseasonalized", 
                         "detrended"))) {
    
        which <- readline("Keyword or position number: ")
        
    }
    cat("\n")
    }
  
    which
}


check_which_hf <- function(which) {
  
    if (is.null(which)) {
    text_prompt <- data.frame(
      c("facets:", "observations:", "fitted:", "residuals:"),
      c(
        "Facet plot of the estimated components",
        "The observed time series",
        "The observations together with estimated trend + seasonality",
        "The both detrended and seasonally adjusted series"
      )
    )
    colnames(text_prompt) <- NULL
    
    cat("\nSelect one of the following plots via the keyword or the position number (exit with 0):\n")
    
    print.data.frame(text_prompt, row.names = FALSE, right = FALSE)
    
    cat("\n")
    
    which <- ""
    
    while(!(which %in% c(0, 1, 2, 3, 4, "facets", "observations", "fitted",
                         "residuals"))) {
    
        which <- readline("Keyword or position number: ")
        
    }
    cat("\n")
    }
  
    which
}

aniSave <- function(fun) {
  switch(
    fun,
    HTML = animation::saveHTML,
    Latex = animation::saveLatex,
    GIF = animation::saveGIF
  )
}

select_arma_orders <- function(xt, ar, ma) {
  
  if (is.null(ar) && is.null(ma)) {
    n <- length(xt)
    bic <- matrix(NA, nrow = 6, ncol = 6)
    P <- Q <- 0:5
    for (p0 in P) {
      for (q0 in Q) {
        arma <- suppressWarnings(stats::arima(xt,
                                              order = c(p0, 0, q0),
                             include.mean = FALSE))
        bic[p0 + 1, q0 + 1] <- -2 * arma$loglik + (p0 + q0) * log(n)
      }
    }
    orders <- c(which(bic == min(bic), arr.ind = TRUE)) - 1
    ar <- orders[[1]]
    ma <- orders[[2]]
  } else if (is.null(ar)) {
    ar <- 0
  } else if (is.null(ma)) {
    ma <- 0
  }
  c(ar, ma)
}