# last revision: 13 Jan 2012

#' @include ChemosensorsClass.R
NULL

#----------------------------
# Class defintion
#----------------------------

#' The function \code{validObject} for class \code{\link{SensorNoiseModel}}.
#'
#' @param object A \code{\link{SensorNoiseModel}} object
#' @return TRUE if the object is valid. Otherwise, a character error string.
#' @name validSensorNoiseModel
#' @rdname int-validSensorNoiseModel
validSensorNoiseModel <- function(object)
{
  if(sum(ssd(object) < 0)) return("'ssd' is negative")    
  
  return(TRUE)
}

#' Class SensorNoiseModel.
#'
#' Class \code{\link{SensorNoiseModel}} generates a sensor noise.
#'
#' Slots of the class:
#' \tabular{rl}{
#'   \code{ssd} \tab Parameter of standard deviation used to generate the noise. \cr
#'   \code{sntype} \tab Noise type. One type is implemented, 'randomWalk'. \cr
#'   \code{snf} \tab Scaling factor for the amplitude to generate the noise.
#'     The default value is c(1, 1, 0.2). \cr
#'   \code{sndata} \tab The reference data of standard deviation values from UNIMAN dataset. \cr
#' }
#' @name SensorNoiseModel
#' @rdname www-SensorNoiseModel
#' @keywords SensorNoiseModel-class
#' @example R/example/SensorNoiseModel-class.R
#' @exportClass SensorNoiseModel
setClass(Class="SensorNoiseModel", 
  representation=representation(
    # class-specific slots
    ssd = "numeric",
    # class-common slots
    idx = "numeric", num = "numeric", gases = "numeric", ngases = "numeric", gnames="character", concUnits="character", concUnitsInt="character",
    # class-specific slots
    sntype = "character", snf = "numeric", sndata = "matrix"),  
  validity=validSensorNoiseModel
)


#----------------------------
# Print/Show Methods
#----------------------------

#' @exportMethod print
setMethod ("print","SensorNoiseModel", function(x, ...)
{
  cat(" Sensor Noise Model\n")
  cat(" - num:", paste(num(x), collapse=", "), "\n")
  cat(" -", ngases(x), "gases", paste(gnames(x), collapse=", "), "\n")
  cat(" - ssd:", paste(ssd(x), collapse=", "), "\n")
  cat(" - noise type:", type(x), "\n")
  cat(" - noise-factor:", paste(noisef(x), collapse=", "), "\n")    
})

#' @exportMethod show
setMethod ("show","SensorNoiseModel", function(object)
{
  cat(" Sensor Noise Model (num ", paste(num(object), collapse=", "), "), (ssd ", 
    paste(ssd(object), collapse=", "), "), ", "noise type '", 
    type(object), "'", "\n", sep='')
})


#----------------------------
# Plot Methods
#----------------------------

#----------------------------
# Get/Set Methods
#----------------------------

setMethod("ssd", "SensorNoiseModel", function(x) x@ssd)
setMethod("type", "SensorNoiseModel", function(x) x@sntype)
setMethod("noisef", "SensorNoiseModel", function(x) x@snf)

setReplaceMethod("ssd", "SensorNoiseModel", function(object, value) 
{
  object@ssd <- value
  validObject(object)
  return (object)
})

#----------------------------
# Model Methods
#----------------------------

#----------------------------
# Predict methods
#----------------------------

#----------------------------
# Noise Methods
#----------------------------
setReplaceMethod("nsd", "SensorNoiseModel", function(object, value) 
{
  ssd(object) <- value
  validObject(object)
  return (object)
})

