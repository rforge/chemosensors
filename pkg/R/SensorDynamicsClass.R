# last revision: 9 Jan 2012

#' @include ChemosensorsClassMethods.R
NULL

#----------------------------
# Support Class defintion
#----------------------------


#----------------------------
# Class defintion
#----------------------------

validSensorDynamics <- function(object)
{
  #if(coeffNonneg(object)) {
  #  coeff <- coef(object)
  #  if(sum(coeff < 0))
  #    return("slot 'coeffNonneg' is TRUE, but some of coefficients are negative.")
  #}
  return(TRUE)
}

#' Class SensorDynamics.
#'
#' Class \code{\link{SensorDynamics}} emulates a temporal dynamics of the sensor as a low-pass filter.
#'
#' Slots of the class:
#' \tabular{rl}{
#'   \code{num} \tab Sensor number (\code{1:17}). The default value is \code{1}. \cr
#'   \code{gases} \tab Gas indices. \cr
#'   \code{ngases} \tab The number of gases. \cr
#'   \code{gnames} \tab Names of gases. \cr
#'   \code{concUnits} \tab Concentration units external to the model. \cr
#'   \code{concUnitsInt} \tab Concentration units internal to the model. \cr
#'   \code{tunit} \tab Time length of the gas pulses. The default value is \code{60}. \cr
#'   \code{enableDyn} \tab A logical value indicates whether time dynamics is enabled. It is \code{TRUE} if \code{tunit} is greater than one. \cr
#'   \code{tconst} \tab Time constants of the low pass filter. \cr
#' }
#'
#' Methods of the class:
#' \tabular{rl}{
#'   \code{predict} \tab Predicts a temporal response of the sensor. \cr
#'   \code{coef} \tab Extracts the time constants of the filter. \cr
#' }
#' 
#' The \code{plot} method has one type (parameter \code{y}):
#' \tabular{rl}{
#'   \code{predict} \tab  Depicts a temporal signal of the model. \cr
#' }
#'
#' @name SensorDynamics-class
#' @rdname SensorDynamics-class
#' @seealso \code{\link{UNIMANtransient}}
#' @example inst/examples/SensorDynamics-class.R
#' @exportClass SensorDynamics
setClass(Class="SensorDynamics", 
  representation=representation(
    # slots common for all classes
    num="numeric",
    gases = "numeric", ngases = "numeric", gind = "numeric", gnames="character",
    concUnits="character", concUnitsInt="character",    
    # class-specific slots
    tunit = "numeric", enableDyn = 'logical',
    ntconst = "numeric", trange = "array",
    tconst = "array"),  
  validity=validSensorDynamics
)

#----------------------------
# Print/Show Methods
#----------------------------

#' @rdname class-methods
#' @aliases print,SensorDynamics-method
setMethod ("print", "SensorDynamics", function(x, ...)
{
  cat(" Sensor Dynamics Model\n")
  cat(" - num", paste(num(x), collapse=", "), "\n")
  cat(" -", ngases(x), "gases", paste(gnames(x), collapse=", "), "\n")
})

setMethod ("show", "SensorDynamics", function(object)
{
  cat(" Sensor Dynamics Model (num ", paste(num(object), collapse=", "), ")", "\n", sep='')
})

#----------------------------
# Get/Set Methods
#----------------------------

#' @rdname get-methods
#' @aliases tunit,SensorDynamics-method
setMethod("tunit", "SensorDynamics", function(x) x@tunit)

#' @rdname SensorDynamics-class
#' @aliases nconst,SensorDynamics-method
setMethod("nconst", "SensorDynamics", function(x) x@nconst)

#' @rdname SensorDynamics-class
#' @aliases trange,SensorDynamics-method
setMethod("trange", "SensorDynamics", function(x) x@trange)

#' @rdname SensorDynamics-class
#' @aliases tconst,SensorDynamics-method
setMethod("tconst", "SensorDynamics", function(x) x@tconst)

#' @rdname get-methods
#' @aliases enableDyn,SensorDynamics-method
setMethod("enableDyn", "SensorDynamics", function(x) x@enableDyn)

#' @name enableDyn<-
#' @aliases enableDyn<-,SensorDynamics-method
#' @rdname set-methods
setReplaceMethod("enableDyn", "SensorDynamics", function(object, value) 
{
  object@enableDyn <- value
  
  validObject(object)
  return(object)
})


#----------------------------
# Plot Methods
#----------------------------


#----------------------------
# Model Methods
#----------------------------

#----------------------------
# Predict methods
#----------------------------

