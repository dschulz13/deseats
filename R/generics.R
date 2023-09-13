#'Smoothing Option Generics
#'
#'Various generics that can be used write methods to either retrieve or set 
#'smoothing options.
#'
#'@param x an object from which to either retrieve options or in which to set 
#'an option.
#'
#'@details
#'\code{order_poly}, \code{season}, \code{kernel_fun}, \code{bwidth}, and
#'\code{boundary_method} can be used to retrieve the order of polyonomial,
#'the seasonal frequency, the kernel function setting, the bandwidth and
#'the boundary method from an object with smoothing options. The corresponding
#'generics beginning with \code{<-} are useful to set such options instead.
#'
#'The generics themselves are without a direct purpose. 
#'
#'@export
methods::setGeneric("order_poly", function(x) standardGeneric("order_poly"))
#'@rdname order_poly
#'
#'@param value the value to set the corresponding option to.
#'
#'@export
methods::setGeneric("order_poly<-", function(x, value) standardGeneric("order_poly<-"))

#'@rdname order_poly
#'@export
methods::setGeneric("season", function(x) standardGeneric("season"))
#'@rdname order_poly
#'@export
methods::setGeneric("season<-", function(x, value) standardGeneric("season<-"))

#'@rdname order_poly
#'@export
methods::setGeneric("kernel_fun", function(x) standardGeneric("kernel_fun"))
#'@rdname order_poly
#'@export
methods::setGeneric("kernel_fun<-", function(x, value) standardGeneric("kernel_fun<-"))

#'@rdname order_poly
#'@export
methods::setGeneric("bwidth", function(x) standardGeneric("bwidth"))
#'@rdname order_poly
#'@export
methods::setGeneric("bwidth<-", function(x, value) standardGeneric("bwidth<-"))

#'@rdname order_poly
#'@export
methods::setGeneric("boundary_method", function(x) standardGeneric("boundary_method"))
#'@rdname order_poly
#'@export
methods::setGeneric("boundary_method<-", function(x, value) standardGeneric("boundary_method<-"))

#------------------------------------------#

#'Obtain Estimated Components of a Time Series
#'
#'Obtain estimated components, such as the estimated trend, the seasonally 
#'adjusted series, and so on from an estimation object.
#'
#'@param x the estimation object.
#'@param ... currently without use; included for future compatibility.
#'
#'@export
methods::setGeneric("trend", function(x, ...) standardGeneric("trend"))

#'@rdname trend
#'
#'
#'
#'@export
methods::setGeneric("deseasonalize", function(x, ...) standardGeneric("deseasonalize"))

#'@rdname trend
#'@export
methods::setGeneric("detrend", function(x, ...) standardGeneric("detrend"))

#------------------------------------------#

#'Automatic Creation of Animations
#'
#'A generic that is the basis for methods that allow the user to create 
#'specific animations on-the-fly.
#'
#'@param object the input object.
#'@param ... currently of no use; included for future compatibility.
#'
#'@export
methods::setGeneric("animate", function(object, ...) standardGeneric("animate"))

#------------------------------------------#

methods::setGeneric(".boot_f_generate", function(object, ...) standardGeneric(".boot_f_generate"))

#'@export
methods::setGeneric("bwidth_interval", function(object, ...) standardGeneric("bwidth_interval"))

#------------------------------------------#

#'Automatic Creation of Animations
#'
#'A generic that is the basis for methods that allow the user to exponentiate 
#'certain results quickly.
#'
#'@param object the input object.
#'@param ... currently of no use; included for future compatibility.
#'
#'@export
methods::setGeneric("expo", function(object, ...) standardGeneric("expo"))