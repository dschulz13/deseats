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
  function(object, n.ahead = 1, ...) {
    freq <- object@nonpar_model@frequency
    trend_e <- utils::tail(trend(object), 2)
    
    times <- ceiling(n.ahead / freq)    
    season_e <- rep(utils::tail(season(object), freq), times)
    
    trend_incr <- trend_e[[2]] - trend_e[[1]]
    
    trend_fc <- trend_e[[2]] + (1:n.ahead) * trend_incr
    
    season_fc <- season_e[1:n.ahead]
    
    arma_fc <- predict(object@par_model, n.ahead = n.ahead, se.fit = FALSE)
    
    trend_fc + season_fc + arma_fc
    
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