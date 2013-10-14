# last revision: 18 Jan 2012

#' @include ConcNoiseModelClass.R
#' @include SensorNoiseModelClass.R
#' @include SensorModelClass.R
#' @include SorptionModelClass.R
NULL

#----------------------------
# Class defintion/constructor
#----------------------------

validSensorArray <- function(object)
{
  return(TRUE)
}

#' Class SensorArray.
#'
#' Class \code{SensorArray} is a extension of the class \code{\link{Sensor}} for many sensor elements.
#'
#' The array aggregates classes \code{\link{ConcNoiseModel}}, \code{\link{SensorNoiseModel}}, 
#' \code{\link{SorptionModel}}, \code{\link{SensorModel}} and \code{\link{DriftNoiseModel}}.
#' 
#' In comparision to the class \code{\link{Sensor}}, slot \code{num} is a numeric vector, 
#' and class \code{SensorArray} also inherits class \code{\link{DriftNoiseModel}}.
#'
#' See \code{\link{Sensor}} and \code{\link{DriftNoiseModel}} for more details.
#' 
#' Slots of the class:
#' \tabular{rl}{
#'   \code{type} \tab Sensor type (not used). Default value is \code{polymeric}. \cr
#'   \code{idx} \tab Sensor index (unique ID number). \cr
#'   \code{enableSorption} \tab Boolean whether \code{\link{SorptionModel}} is enabled. Default value is \code{TRUE}. \cr
#'   \code{...} \tab Slots inherited from super-classes \code{\link{ConcNoiseModel}}, \code{\link{SensorNoiseModel}}, 
#'     \code{\link{SorptionModel}}, \code{\link{SensorModel}} and \code{\link{DriftNoiseModel}}. \cr
#' }
#'
#' Methods of the class:
#' \tabular{rl}{
#'   \code{predict} \tab Predicts a model response to an input concentration matrix. \cr
#'   \code{coef} \tab Extracts the coefficient matrix from sensors. \cr
#'   \code{csd} \tab Gets the concentration noise level (inherited from class \code{\link{ConcNoiseModel}}). \cr
#'   \code{csd<-} \tab Sets the concentration noise level. \cr
#'   \code{ssd} \tab Gets the sensor noise level (inherited from class \code{\link{SensorNoiseModel}}). \cr
#'   \code{ssd<-} \tab Sets the sensor noise level. \cr
#' }
#' 
#' The \code{plot} method has the only type (parameter \code{y}):
#' \tabular{rl}{
#'   \code{response} \tab (default) Shows the sensitivity curves per gas in normalized concentration units. \cr
#' }
#'
#' @name SensorArray-class
#' @rdname SensorArray-class
#' @seealso \code{\link{Sensor}}, \code{\link{DriftNoiseModel}}
#' @example inst/examples/SensorArray-class.R
#' @exportClass SensorArray
setClass(Class="SensorArray", 
  representation = representation(
    type = "character", idx = "numeric",
    enableSorption = "logical"),  
  contains = subClasses("SensorArray"), # from 'ChemoSensorArraysClassMethods.R'
  validity=validSensorArray
)

#----------------------------
# Print/Show Methods
#----------------------------

#' @rdname class-methods
#' @aliases print,SensorArray-method
setMethod ("print","SensorArray", function(x, ...)
{
  cat(" SensorArray\n")
  cat(" - enableSorption:", enableSorption(x), "\n")  
  # print sub classes
  sub.classes <- subClasses(class(x)) # from 'ChemoSensorArraysClassMethods.R'
  for(i in seq_along(sub.classes)) {
    cl <- sub.classes[i]
    cat(" (", i, ")", sep='')
    print(as(x, cl))
  }
})

setMethod ("show", "SensorArray", function(object)
{
  cat(" Sensor Array of ", nsensors(object), " sensors, ", 
    ngases(object), " gases ", paste(gnames(object), collapse=", "), "\n", sep='')
  cat(" - enableSorption ", enableSorption(object), ", enableDyn ", enableDyn(object), "\n", sep='')
  # print sub classes
  sub.classes <- subClasses(class(object)) # from 'ChemoSensorArraysClassMethods.R'
  for(cl in sub.classes) {
    cat(" -")
    show(as(object, cl))
  }
#  cat(" SensorArray\n")
#  cat(" - num (", object@num, "), knum(", object@knum, ")\n", sep="")
#  cat(" - time dynamics:", " enable (", object@enableDyn, ")", "\n", sep="")
#  cat(" - sorption:", " K-affinity (", paste(round(object@smodel$K, 2), collapse=", "), ")", "\n", sep="")
#  cat(" - noise:", " csd (", object@csd, "),", " ssd (", object@ssd, "),", " sstep (", object@sstep, ")", "\n", sep="")
})

#----------------------------
# Plot Methods
#----------------------------

#----------------------------
# Model Methods
#----------------------------

#----------------------------
# Get/Set methods
#----------------------------

#' @rdname get-methods
#' @aliases enableSorption,SensorArray-method
setMethod("enableSorption", "SensorArray", function(x) x@enableSorption)

#----------------------------
# Noise Methods
#----------------------------

#' @name nsd<-
#' @aliases nsd<-,SensorArray-method
#' @rdname noise-methods
setReplaceMethod("nsd", "SensorArray", function(object, value) 
{
  csd(object) <- value
  ssd(object) <- value
  dsd(object) <- value
    
  validObject(object)
  return (object)
})
