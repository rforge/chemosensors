# last revision: 9 Jan 2012

#' @include ChemosensorsClassMethods.R
NULL

#----------------------------
# Support Class defintion
#----------------------------


#----------------------------
# Class defintion
#----------------------------

#' The function \code{validObject} for class \code{\link{SensorModel}}.
#'
#' @param object A \code{\link{SensorModel}} object
#' @return TRUE if the object is valid. Otherwise, a character error string.
#' @name validSensorModel
#' @rdname int-validSensorModel
validSensorModel <- function(object)
{
  if(coeffNonneg(object)) {
    coeff <- coef(object)
    if(sum(coeff < 0))
      return("slot 'coeffNonneg' is TRUE, but some of coefficients are negative.")
  }
  return(TRUE)
}

#' Class SensorModel.
#'
#' Class \code{\link{SensorModel}} predicts a sensor signal in response to an input concentration matrix
#' by means of a data model in slot \code{model}.
#'
#' The training of the data model is based on the reference data from a dataset
#' measured at The University of Manchester (UNIMAN) for three analytes 
#' ammonia, propanoic acid and n-buthanol at different concentration levels.
#'
#' Slots of the class:
#' \tabular{rl}{
#'   \code{num} \tab Sensor number (1:17). The default value is 1. \cr
#'   \code{gases} \tab Gas indices. \cr
#'   \code{ngases} \tab The number of gases. \cr
#'   \code{gnames} \tab Names of gases. \cr
#'   \code{concUnits} \tab Concentration units external to the model, values given in an input concentration matrix. \cr
#'   \code{concUnitsInt} \tab Concentration units internal for the model. \cr
#'   \code{conc} \tab List of characteristic concentration values per gas (in the adsorbed form):
#'       \code{min}, \code{max}, \code{crit}, \code{sat}. \cr
#'   \code{conc0} \tab List of characteristic concentration values per gas (in air):
#'       \code{min}, \code{max}, \code{crit}, \code{sat}. \cr
#'   \code{dataModel} \tab Data model of class \code{\link{SensorDataModel}}. \cr
#'   \code{coeffNonneg} \tab Logical whether model coefficients must be non-negative. By default, FALSE. \cr
#'   \code{coeffNonnegTransform} \tab Name of transformation to convert negative model coefficients to non-negative values. \cr
#' }
#' @name SensorModel
#' @rdname www-SensorModel
#' @keywords SensorModel-class
#' @exportClass SensorModel
setClass(Class="SensorModel", 
  representation=representation(
    num="numeric",
    gases = "numeric", ngases = "numeric", gind = "numeric", gnames="character", 
    concUnits="character", concUnitsInt="character",
    conc = "list", conc0 = "list",
    dataModel = "SensorDataModel", coeffNonneg = "logical", coeffNonnegTransform = "character"),  
  validity=validSensorModel
)

#----------------------------
# Print/Show Methods
#----------------------------

#' @exportMethod print
setMethod ("print","SensorModel", function(x, ...)
{
  cat(" Sensor Model\n")
  cat(" - num", num(x), "\n")
  cat(" -", ngases(x), "gases", paste(gnames(x), collapse=", "), "\n")
  print(x@dataModel)
})

#' @exportMethod show
setMethod ("show","SensorModel", function(object)
{
  cat(" Sensor Model (num ", num(object), "), ", "data model '", modelName(object), "'", "\n", sep='')
})

#----------------------------
# Get/Set Methods
#----------------------------

setMethod("coeffNonneg", "SensorModel", function(x) x@coeffNonneg)

setMethod("modelName", "SensorModel", function(x) x@dataModel$method)

#----------------------------
# Plot Methods
#----------------------------


#----------------------------
# Model Methods
#----------------------------

#----------------------------
# Predict methods
#----------------------------

