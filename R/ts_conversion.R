#'Time Series Object Conversion from zoo to ts
#'
#'Allows for the conversion of time series objects of class \code{zoo}
#'to time series objects of class \code{ts}. This is only suitable, if
#'the time series is observed in regular time intervals (monthly, quarterly,
#'yearly, etc.). The correct observation time points are then kept.
#'
#'@param xt a time series object of class \code{zoo} with equidistant 
#'observation time points (monthly, quarterly, yearly, etc.).
#'
#'@export
#'
#'@importFrom zoo as.yearmon zoo coredata
#'
#'@importFrom stats as.ts
#'

zoo_to_ts <- function(xt) {
  d <- zoo::as.yearmon(time(xt))
  xt <- zoo::zoo(zoo::coredata(xt), order.by = d)
  stats::as.ts(xt)
}
