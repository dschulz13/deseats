setOldClass("Arima")

setClass("s_semiarma",
  contains = "decomp",
  slots = c(
    nonpar_model = "deseats",
    par_model = "Arima"
  )
)

create_s_semiarma <- function(decomp, nonpar_model, par_model) {

  methods::new("s_semiarma",
    decomp = decomp,
    nonpar_model = nonpar_model,
    par_model = par_model,
    ts_name = nonpar_model@ts_name,
    frequency = nonpar_model@frequency
    
  )

}

#---------------

#'@export
methods::setMethod("predict", "s_semiarma", 
  function(object, n.ahead = 1, intervals = TRUE, alpha = c(0.95, 0.99), method = c("norm", "boot"), 
           expo = FALSE, ...) {
    
    if (intervals) {
    
      if (!(length(method) %in% c(1, 2)) || !all(!is.na(method)) ||
          !is.character(method)) {
        stop("The argument 'method' is defined incorrectly.")
      }
      if (all(method == c("norm", "boot"))) method <- "norm"   
    
    }
    
    freq <- object@nonpar_model@frequency
    trend_e <- utils::tail(trend(object), 2)
    
    times <- ceiling(n.ahead / freq)    
    season_e <- rep(utils::tail(season(object), freq), times)
    
    trend_incr <- trend_e[[2]] - trend_e[[1]]
    
    trend_fc <- trend_e[[2]] + (1:n.ahead) * trend_incr
    
    season_fc <- season_e[1:n.ahead]
    
    arma_fc <- predict(object@par_model, n.ahead = n.ahead, se.fit = FALSE)
    
    fc_total <- trend_fc + season_fc + arma_fc
    
    if (intervals) {
      
      p <- object@par_model$arma[[1]]
      q <- object@par_model$arma[[2]]
      
      if (method == "norm") {
        
        fi <- normCast(model = object@par_model,
          p = p, q = q, h = n.ahead, alpha = alpha)        
        
      } else if (method == "boot") {
        
        fi <- bootCast(model = object@par_model, X = object@decomp[, 4],
          p = p, q = q, h = n.ahead, alpha = alpha)

      }
      
      fi_names = colnames(fi)

      fi <- ts(fi, start = c(time(fc_total)[[1]]), frequency = frequency(fc_total))      
      
      interv <- fc_total + fi

      
    } else {
      
      interv <- ts(NA_real_)
      
    }

    obs <- object@decomp[, 1]   
    ts_name <- object@nonpar_model@ts_name    
    
    if (expo) {
      fc_total <- exp(fc_total)
      interv <- exp(interv)
      obs <- exp(obs)
      ts_name <- paste0("exp(", ts_name, ")")
    }
    
    out <- create_deseats_fc(
      pred = fc_total,
      interv = interv,
      obs = obs,
      ts_name = ts_name
    )
    
    colnames(out@interv) <- fi_names
    
    out
    
  }
)

#'@export
methods::setMethod("show", "s_semiarma", 
  function(object) {
    text_out <- paste0(
    "\n*******************************************\n",
    "*                                         *\n",
    "*        Fitted Seasonal Semi-ARMA        *\n",
    "*                                         *\n",    
    "*******************************************\n\n",
    
    "Series: ", object@ts_name, "\n\n",
    
    "Nonparametric part (Trend + Seasonality):\n",
    "-----------------------------------------\n\n",
    "  Bandwidth: ", sprintf("%.4f", object@nonpar_model@bwidth), "\n\n",
    "  Trend:\n",
    "  ------\n",
    "    Order of local polynomial: ", object@nonpar_model@order_poly, "\n\n",
    "  Seasonality:\n",
    "  ------------\n",
    "    Frequency: ", object@nonpar_model@frequency, "\n\n",   
    "Parametric part (Rest):\n",
    "-----------------------\n",
    paste0(utils::capture.output(print(object@par_model)), collapse = "\n"), "\n\n"
  )
  cat(text_out)
}
)

#'@export
setMethod("bwidth", "s_semiarma",
  function(x){
    x@nonpar_model@bwidth
  }
)