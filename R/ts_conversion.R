#'Time Series Object Conversion from \code{"zoo"} to \code{"ts"}
#'
#'Allows for the conversion of time series objects of class \code{"zoo"}
#'to time series objects of class \code{"ts"}. This is only suitable, if
#'the time series is observed in regular time intervals (monthly, quarterly,
#'yearly, etc.). The correct observation time points are then kept.
#'
#'@param xt a time series object of class \code{"zoo"} with equidistant 
#'observation time points (monthly, quarterly, yearly, etc.).
#'
#'@details
#'An equidistant time series object of class \code{"zoo"} is transformed to
#'class \code{"ts"}. This is particularly useful, since most functions of this
#'package work with objects of class \code{"ts"} only.
#'
#'@return
#'An object of class \code{"ts"} is returned.
#'
#'@export
#'
#'@importFrom zoo as.yearmon zoo coredata
#'
#'@importFrom stats as.ts
#'
#'@examples
#'# Create example zoo-object
#'tp <- seq(from = as.Date("2020-01-01"), to = as.Date("2020-10-01"), by = "month")
#'xt <- zoo::zoo(1:10, order.by = tp)
#'xt
#'
#'# Transform into ts-object
#'yt <- zoo_to_ts(xt)
#'yt
#'

zoo_to_ts <- function(xt) {
  d <- zoo::as.yearmon(time(xt))
  xt <- zoo::zoo(zoo::coredata(xt), order.by = d)
  stats::as.ts(xt)
}
