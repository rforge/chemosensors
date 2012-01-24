# last revision: 23 Jan 2012

#' @include DriftNoiseModelClassMethods.R
#' @include ChemosensorsClassMethods.R
NULL

#----------------------------
# Support Class defintion
#----------------------------


#----------------------------
# Class defintion
#----------------------------

#' The function \code{validObject} for class \code{\link{DriftNoiseModel}}.
#'
#' @param object A \code{\link{DriftNoiseModel}} object
#' @return TRUE if the object is valid. Otherwise, a character error string.
#' @name validDriftNoiseModel
#' @rdname int-validDriftNoiseModel
validDriftNoiseModel <- function(object)
{
  return(TRUE)
}

#' Class DriftNoiseModel.
#'
#' Class \code{\link{DriftNoiseModel}} generates the drift noise in a multi-variate manner.
#'
#' Slots of the class:
#' \tabular{rl}{
#'   \code{num} \tab Sensor number (1:17). The default value is 1. \cr
#'   \code{dsd} \tab Parameter sd for drift noise. A number from 0 to 1. The default value is 0.1. \cr
#'   \code{ndcomp} \tab The number of drift components. The default number is 3. \cr
#'   \code{ndvar} \tab The importance values of drift components. The default values are taken from the UNIMAN multi-variate distribution. \cr
#'   \code{driftModel} \tab Drift model of class \code{\link{DriftCommonModel}}. \cr
#' }
#' @name DriftNoiseModel
#' @rdname www-DriftNoiseModel
#' @example R/example/DriftNoiseModel-class.R
#' @keywords DriftNoiseModel-class
#' @exportClass DriftNoiseModel
setClass(Class="DriftNoiseModel", 
  representation=representation(
    num = "numeric",
    dsd = "numeric", ndcomp = "numeric", ndvar = "numeric",
    driftModel = "DriftCommonModel"),  
  validity=validDriftNoiseModel
)

#----------------------------
# Print/Show Methods
#----------------------------

#' @exportMethod print
setMethod ("print","DriftNoiseModel", function(x, ...)
{
  cat(" Drift Noise Model\n")
  cat(" - num", num(x), "\n")
  print(x@driftModel)
})

#' @exportMethod show
setMethod ("show","DriftNoiseModel", function(object)
{
  cat(" Drift Noise Model (num ", paste(num(object), collapse=", "), "), ", "common model '", modelName(object), "'", "\n", sep='')
})

#----------------------------
# Get/Set Methods
#----------------------------

setMethod("modelName", "DriftNoiseModel", function(x) x@driftModel$method)

setMethod("dsd", "DriftNoiseModel", function(x) x@dsd)
setMethod("dspace", "DriftNoiseModel", function(x) x@driftModel$dspace[, 1:x@driftModel$ndcomp])
setMethod("ndcomp", "DriftNoiseModel", function(x) x@ndcomp)
setMethod("ndvar", "DriftNoiseModel", function(x) x@ndvar)

setReplaceMethod("dsd", "DriftNoiseModel", function(object, value) 
{
  object@dsd <- value
  validObject(object)
  return (object)
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

