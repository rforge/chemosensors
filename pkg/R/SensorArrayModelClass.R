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
#' Class \code{\link{SensorArrayModel}} is a extension of the class \code{\link{SensorModel}}
#' for many sensor elements.
#'
#' In comparision to the class \code{\link{SensorModel}}, 
#' slot \code{dataModel} is a list containig the data models of class \code{\link{SensorDataModel}}.
#'
#' See \code{\link{SensorModel}} for more details.
#' 
#' Slots of the class:
#' \tabular{rl}{
#'   \code{num} \tab Sensor number (\code{1:17}). The default value is \code{c(1, 2)}. \cr
#'   \code{gases} \tab Gas indices. \cr
#'   \code{ngases} \tab The number of gases. \cr
#'   \code{gnames} \tab Names of gases. \cr
#'   \code{concUnits} \tab Concentration units external to the model, values given in an input concentration matrix. \cr
#'   \code{concUnitsInt} \tab Concentration units internal for the model, values used numerically to build regression models. \cr
#'   \code{dataModel} \tab List of data models of class \code{\link{SensorDataModel}}. \cr
#'   \code{coeffNonneg} \tab Logical whether model coefficients must be non-negative. By default, \code{FALSE}. \cr
#'   \code{coeffNonnegTransform} \tab Name of transformation to convert negative model coefficients to non-negative values. \cr
#' }
#'
#' Methods of the class:
#' \tabular{rl}{
#'   \code{predict} \tab Predicts a model response to an input concentration matrix. \cr
#'   \code{coef} \tab Extracts the coefficients of models stored in slot \code{dataModel}. \cr
#' }
#' 
#' The \code{plot} method has the only type (parameter \code{y}):
#' \tabular{rl}{
#'   \code{response} \tab (default) Shows the sensitivity curves per gas in normalized concentration units. \cr
#' }
#'
#' @name SensorArrayModel
#' @rdname www-SensorArrayModel
#' @keywords SensorArrayModel
#' @seealso \code{\link{SensorModel}}
#' @example R/example/SensorArrayModel-class.R
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
  cat(" - (first)")
  print(x@dataModel[[1]])
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

