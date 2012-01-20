# last revision: 10 Jan 2012

#' @include ConcNoiseModelClass.R
#' @include SensorNoiseModelClass.R
#' @include SensorModelClass.R
#' @include SorptionModelClass.R
NULL

#----------------------------
# Class defintion/constructor
#----------------------------

#' The function \code{validObject} for class \code{\link{Sensor}}.
#'
#' @param object A \code{\link{Sensor}} object
#' @return TRUE if the object is valid. Otherwise, a character error string.
#' @name validSensorModel
#' @rdname int-validSensorModel
validSensor <- function(object)
{
  return(TRUE)
}

#' Class Sensor.
#'
#' Class \code{\link{Sensor}} predicts a sensor signal in response to an input concentration matrix
#' by passing the data through a list of models.
#'
#' The models aggregated into the class are {\link{ConcNoiseModel}}, {\link{SensorNoiseModel}} and {\link{SensorModel}}.
#' 
#' Slots of the class:
#' \tabular{rl}{
#'   \code{type} \tab Sensor type. Default value is 'polymeric'. \cr
#'   \code{idx} \tab Sensor index (unique ID number). \cr
#' }
#' @name Sensor
#' @rdname www-Sensor
#' @keywords Sensor-class
#' @example R/example/Sensor-class.R
#' @exportClass Sensor
setClass(Class="Sensor", 
  representation = representation(
    type = "character", idx = "numeric",
    enableSorption = "logical"),  
  contains = subClasses("Sensor"), # from 'ChemosensorsClassMethods.R'
  validity=validSensor
)


#----------------------------
# Print/Show Methods
#----------------------------

#' @exportMethod print
setMethod ("print","Sensor", function(x, ...)
{
  cat(" Sensor\n")
  cat(" - enableSorption:", enableSorption(x), "\n")  
  # print sub classes
  sub.classes <- subClasses(class(x)) # from 'ChemosensorsClassMethods.R'
  for(i in seq_along(sub.classes)) {
    cl <- sub.classes[i]
    cat(" (", i, ")", sep='')
    print(as(x, cl))
  }
})

#' @exportMethod show
setMethod ("show", "Sensor", function(object)
{
  cat(" Sensor (enableSorption ", enableSorption(object), ")", "\n", sep='')
  # print sub classes
  sub.classes <- subClasses(class(object)) # from 'ChemosensorsClassMethods.R'
  for(cl in sub.classes) {
    cat(" -")
    show(as(object, cl))
  }
#  cat(" Sensor\n")
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
setMethod("enableSorption", "Sensor", function(x) x@enableSorption)

#----------------------------
# Noise Methods
#----------------------------
setReplaceMethod("nsd", "Sensor", function(object, value) 
{
  csd(object) <- value
  ssd(object) <- value
  
  validObject(object)
  return (object)
})
