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
#' by calling two sub-models \code{\link{SorptionModel}} and \code{\link{SensorModel}}. 
#'
#' The sensor aggregates classes \code{\link{ConcNoiseModel}}, \code{\link{SensorNoiseModel}}, 
#' \code{\link{SorptionModel}} and \code{\link{SensorModel}}.
#' 
#' The sorption and calibration models (classes \code{\link{SorptionModel}} and \code{\link{SensorModel}})
#' have different roles in the simulation flow. 
#' The sorption model controls the the amount of gas absorbed by the sensor following the Langmuir
#' relation, and that emulates the non-linearity nature intrinsic for
#' the polymeric sensors. The calibration model regulates the relationship between this amount of the absorbed gas 
#' and the output sensor signal. Both sorption and calibration models utilize
#' the short-term reference data \code{\link{UNIMANshort}} for parameter estimation.
#' The sorption model can be disabled by parameter \code{enableSorption}.
#'
#' Slots of the class:
#' \tabular{rl}{
#'   \code{type} \tab Sensor type (not used). Default value is \code{polymeric}. \cr
#'   \code{idx} \tab Sensor index (unique ID number). \cr
#'   \code{enableSorption} \tab Boolean whether \code{\link{SorptionModel}} is enabled. Default value is \code{TRUE}. \cr
#'   \code{...} \tab Slots inherited from super-classes \code{\link{ConcNoiseModel}}, \code{\link{SensorNoiseModel}}, \code{\link{SorptionModel}} 
#'     and \code{\link{SensorModel}}. \cr
#' }
#'
#' Methods of the class:
#' \tabular{rl}{
#'   \code{predict} \tab Predicts a sensor response to an input concentration matrix. \cr
#'   \code{coef} \tab Extracts the coefficients inherited from class \code{\link{SensorModel}}. \cr
#'   \code{csd} \tab Gets the concentration noise level (inherited from class \code{\link{ConcNoiseModel}}). \cr
#'   \code{csd<-} \tab Sets the concentration noise level. \cr
#'   \code{ssd} \tab Gets the sensor noise level (inherited from class \code{\link{SensorNoiseModel}}). \cr
#'   \code{ssd<-} \tab Sets the sensor noise level. \cr
#' }
#' 
#' The \code{plot} method has two types (parameter \code{y}):
#' \tabular{rl}{
#'   \code{response} \tab (default) Shows the sensitivity curves per gas in normalized concentration units. \cr
#'   \code{noise} \tab Depicts the noise induced in the output signal of the sensor. \cr
#' }
#'
#' @name Sensor
#' @rdname www-Sensor
#' @keywords Sensor
#' @seealso \code{\link{ConcNoiseModel}}, \code{\link{SensorNoiseModel}}, \code{\link{SorptionModel}}, \code{\link{SensorModel}}
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
