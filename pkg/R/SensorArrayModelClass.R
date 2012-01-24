# last revision: 9 Jan 2012

#' @include ChemosensorsClass.R
NULL

#----------------------------
# Support Class defintion
#----------------------------


#----------------------------
# Class defintion
#----------------------------

#' The function \code{validObject} for class \code{\link{SensorArrayModel}}.
#'
#' @param object A \code{\link{SensorArrayModel}} object
#' @return TRUE if the object is valid. Otherwise, a character error string.
#' @name validSensorArrayModel
#' @rdname int-validSensorArrayModel
validSensorArrayModel <- function(object)
{
  if(coeffNonneg(object)) {
    coeff <- coef(object)
    if(sum(coeff < 0))
      return("slot 'coeffNonneg' is TRUE, but some of coefficients are negative.")
  }
  return(TRUE)
}

#' Class SensorArrayModel.
#'
#' Class \code{\link{SensorArrayModel}} predicts a sensor signal in response to an input concentration matrix
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
#' @name SensorArrayModel
#' @rdname www-SensorArrayModel
#' @keywords SensorArrayModel-class
#' @exportClass SensorArrayModel
setClass(Class="SensorArrayModel", 
  representation=representation(
    num="numeric", idx="numeric",
    gases = "numeric", ngases = "numeric", gind = "numeric", gnames="character", 
    concUnits="character", concUnitsInt="character",
    conc = "list", conc0 = "list",
    dataModel = "list", coeffNonneg = "logical", coeffNonnegTransform = "character"),  
  validity=validSensorArrayModel
)

#----------------------------
# Print/Show Methods
#----------------------------

#' @exportMethod print
setMethod ("print","SensorArrayModel", function(x, ...)
{
  cat(" Sensor Array Model\n")
  cat(" - num", paste(num(x), collapse=", "), "\n")
  cat(" -", ngases(x), "gases", paste(gnames(x), collapse=", "), "\n")
  print(x@dataModel)
})

#' @exportMethod show
setMethod ("show","SensorArrayModel", function(object)
{
  cat(" Sensor Array Model (num ", paste(num(object), collapse=", "), "), ", "data model '", modelName(object), "'", "\n", sep='')
})

#----------------------------
# Get/Set Methods
#----------------------------

setMethod("coeffNonneg", "SensorArrayModel", function(x) x@coeffNonneg)

setMethod("modelName", "SensorArrayModel", function(x) x@dataModel[[1]]$method)

#----------------------------
# Plot Methods
#----------------------------


#----------------------------
# Model Methods
#----------------------------

#----------------------------
# Predict methods
#----------------------------

