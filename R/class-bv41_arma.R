setClass("bv41_arma",
  contains = "decomp",
  slots = c(
    nonpar_model = "bv41",
    par_model = "Arima"
  )
)

create_bv41_arma <- function(decomp, nonpar_model, par_model) {

  methods::new("bv41_arma",
    decomp = decomp,
    nonpar_model = nonpar_model,
    par_model = par_model,
    ts_name = nonpar_model@ts_name,
    frequency = nonpar_model@frequency
  )

}

#'Printing of Fitted BV4.1 Model with ARMA Rest
#'
#'A simple printing method that allows to print objects of class 
#'\code{"bv41_arma"}.
#'
#'@param object an object of class \code{"bv41_arma"} as returned by 
#'\code{\link{BV4.1_arma}}.
#'
#'@details
#'Print details of the results returned by \code{\link{BV4.1_arma}}.
#'
#'@return
#'This method returns \code{NULL}.
#'
#'@author
#'\itemize{
#'\item Dominik Schulz (Research Assistant) (Department of Economics, Paderborn
#'University), \cr
#'Author and Package Creator
#'}
#'
#'@export
#'
#'@examples
#'est <- BV4.1_arma(log(EXPENDITURES))
#'est
#'
setMethod("show", "bv41_arma", function(object) {
  
  invisible(NULL)
  
})

#'Forecasting According to BV4.1 + ARMA
#'
#'Obtain point and interval forecasts based on a fitted BV4.1 model 
#'with ARMA rest.
#'

#'@param object an object of class \code{"s_semiarma"}.
#'@param n.ahead a numeric vector of length one that represents the forecasting horizon; assume that \code{object} is based on 
#'observations at time points \eqn{t=1,2,\dots,n}; forecasts are then obtained for time 
#'points \eqn{t=n+1,n+2,\dots,n+\code{n.ahead}}; the default is \code{n.ahead = 1}..
#'@param ... further arguments of \code{\link{predict,s_semiarma-method}}.
#'
#'@details
#'Assume a BV4.1 model with ARMA errors was fitted using 
#'\code{\link{BV4.1_arma}}.
#'Pass the resulting object to this method, in order to obtain point and 
#'interval forecasts.
#'
#'@export
#'
#'@return
#'A list with the following elements is returned.
#'\describe{
#'\item{\code{pred}}{the obtained point forecasts.}
#'\item{\code{interv}}{the obtained forecasting intervals.}
#'\item{\code{obs}}{the observation series.}
#'\item{\code{ts_name}}{the name of the observation series object.}
#'}
#'
#'@author
#'\itemize{
#'\item Dominik Schulz (Research Assistant) (Department of Economics, Paderborn
#'University), \cr
#'Author and Package Creator
#'}
#'
#'@examples
#'xt <- log(EXPENDITURES)
#'est <- BV4.1_arma(xt)
#'predict(est, n.ahead = 10)
#'
setMethod("predict", "bv41_arma", function(object, n.ahead = 1, ...) {
  
  .f <- methods::selectMethod("predict", "s_semiarma")
  .f(object, n.ahead, ...)  
  
})

