# last revision: 18 Jan 2012

#' @include ConcNoiseModelClass.R
#' @include SensorNoiseModelClass.R
#' @include SensorArrayModelClass.R
#' @include SorptionModelClass.R
NULL

#----------------------------
# Class defintion/constructor
#----------------------------

#' The function \code{validObject} for class \code{\link{SensorArray}}.
#'
#' @param object A \code{\link{SensorArray}} object
#' @return TRUE if the object is valid. Otherwise, a character error string.
#' @name validSensorArray
#' @rdname int-validSensorArray
validSensorArray <- function(object)
{
  return(TRUE)
}

#' Class SensorArray.
#'
#' Class \code{\link{SensorArray}} predicts a sensor array signal in response to an input concentration matrix
#' by passing the data through a list of models.
#'
#' The models aggregated into the class are {\link{ConcNoiseModel}}, {\link{SensorNoiseModel}} and {\link{SensorArrayModel}}.
#' 
#' @name SensorArray
#' @rdname www-SensorArray
#' @keywords SensorArray-class
#@example R/example/SensorArray-class.R
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

#' @exportMethod print
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

#' @exportMethod show
setMethod ("show", "SensorArray", function(object)
{
  cat(" SensorArray (enableSorption ", enableSorption(object), ")", "\n", sep='')
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
setMethod("enableSorption", "SensorArray", function(x) x@enableSorption)

#----------------------------
# Noise Methods
#----------------------------
setReplaceMethod("nsd", "SensorArray", function(object, value) 
{
  csd(object) <- value
  ssd(object) <- value
  
  validObject(object)
  return (object)
})
