#'@export
methods::setGeneric("order_poly", function(x) standardGeneric("order_poly"))
#'@export
methods::setGeneric("order_poly<-", function(x, value) standardGeneric("order_poly<-"))

#'@export
methods::setGeneric("season", function(x) standardGeneric("season"))
#'@export
methods::setGeneric("season<-", function(x, value) standardGeneric("season<-"))

#'@export
methods::setGeneric("kernel_fun", function(x) standardGeneric("kernel_fun"))
#'@export
methods::setGeneric("kernel_fun<-", function(x, value) standardGeneric("kernel_fun<-"))

#'@export
methods::setGeneric("bwidth", function(x) standardGeneric("bwidth"))
#'@export
methods::setGeneric("bwidth<-", function(x, value) standardGeneric("bwidth<-"))

#'@export
methods::setGeneric("boundary_method", function(x) standardGeneric("boundary_method"))
#'@export
methods::setGeneric("boundary_method<-", function(x, value) standardGeneric("boundary_method<-"))

#'@export
methods::setGeneric("correction_factor", function(x) standardGeneric("correction_factor"))
#'@export
methods::setGeneric("correction_factor<-", function(x, value) standardGeneric("correction_factor<-"))

#------------------------------------------#

#'@export
methods::setGeneric("trend", function(x) standardGeneric("trend"))

#'@export
methods::setGeneric("deseasonalize", function(x, ...) standardGeneric("deseasonalize"))

#'@export
methods::setGeneric("detrend", function(x, ...) standardGeneric("detrend"))

#------------------------------------------#

#'@export
methods::setGeneric("animate", function(object, ...) standardGeneric("animate"))

#------------------------------------------#

methods::setGeneric(".boot_f_generate", function(object, ...) standardGeneric(".boot_f_generate"))

#'@export
methods::setGeneric("bwidth_interval", function(object, ...) standardGeneric("bwidth_interval"))

#------------------------------------------#

#'@export
methods::setGeneric("expo", function(object) standardGeneric("expo"))