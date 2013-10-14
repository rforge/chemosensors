# last revision: 9 Jan 2012

#' @include SensorDynamicsClass.R
NULL

#----------------------------
# Support Class defintion
#----------------------------


#----------------------------
# Class defintion
#----------------------------

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
#' by means of a regression model stored in slot \code{dataModel}.
#'
#' The model explicitely assumes that the sensor response to a mixture of 
#' analytes is a sum of responses to the individual analyte components.
#' Linear models \code{mvr} and \code{plsr} follow this assumtion in their nature.
#'
#' Slots of the class:
#' \tabular{rl}{
#'   \code{num} \tab Sensor number (\code{1:17}). The default value is \code{1}. \cr
#'   \code{gases} \tab Gas indices. \cr
#'   \code{ngases} \tab The number of gases. \cr
#'   \code{gnames} \tab Names of gases. \cr
#\code{gind} \tab Gas indicies. \cr
#'   \code{concUnits} \tab Concentration units external to the model, values given in an input concentration matrix. \cr
#'   \code{concUnitsInt} \tab Concentration units internal for the model, values used numerically to build regression models. \cr
#\code{conc} \tab List of characteristic concentration values per gas (in the adsorbed form):
#       \code{min}, \code{max}, \code{crit}, \code{sat}. \cr
#\code{conc0} \tab List of characteristic concentration values per gas (in air):
#       \code{min}, \code{max}, \code{crit}, \code{sat}. \cr
#'   \code{dataModel} \tab Data model of class \code{SensorDataModel} performs a regression (free of the routine on units convertion, etc). \cr
#'   \code{coeffNonneg} \tab Logical whether model coefficients must be non-negative. By default, \code{FALSE}. \cr
#'   \code{coeffNonnegTransform} \tab Name of transformation to convert negative model coefficients to non-negative values. \cr
#'   \code{beta} \tab (parameter of sensor diversity) A scaling coefficient of how different coefficients 
#'     of \code{SensorDataModel} will be in comparision with those coefficients of the UNIMAN sensors. 
#'     The default value is \code{2}. \cr
#' }
#'
#' Methods of the class:
#' \tabular{rl}{
#'   \code{predict} \tab Predicts a sensor model response to an input concentration matrix. \cr
#'   \code{coef} \tab Extracts the coefficients of a regression model stored in slot \code{dataModel}. \cr
#' }
#' 
#' The \code{plot} method has two types (parameter \code{y}):
#' \tabular{rl}{
#'   \code{response} \tab (default) Shows the sensitivity curves per gas in normalized concentration units. \cr
#'   \code{predict} \tab  Depicts input (parameter \code{conc}) and ouput of the model for a specified gas (parameter \code{gases}). \cr
#' }
#'
#' @name SensorModel-class
#' @rdname SensorModel-class
#' @seealso \code{\link{UNIMANshort}}
#' @example inst/examples/SensorModel-class.R
#' @exportClass SensorModel
setClass(Class="SensorModel", 
  representation=representation(
    beta = "numeric",
    num="numeric", fnum="numeric", idx="numeric",
    gases = "numeric", ngases = "numeric", gind = "numeric", gnames="character", 
    concUnits="character", concUnitsInt="character",
    conc = "list", conc0 = "list",
    dataModel = "list", coefsd = "matrix",  
    coeffNonneg = "logical", coeffNonnegTransform = "character"),  
  contains = subClasses("SensorModel"), # from 'ChemoSensorArraysClassMethods.R'    
  validity=validSensorModel
)

#----------------------------
# Print/Show Methods
#----------------------------

#' @rdname class-methods
#' @aliases print,SensorModel-method
setMethod ("print", "SensorModel", function(x, ...)
{
  cat(" Sensor Model\n")
  cat(" - num", numStr(x), "\n")
  cat(" - beta", x@beta, "\n")
  cat(" -", ngases(x), "gases", paste(gnames(x), collapse=", "), "\n")
  cat(" - (first)")
  print(x@dataModel[[1]])
  cat(" - sensor model: ", "coeffNonneg ", coeffNonneg(x), "\n", sep="")
  cat("   -- coefficients (first):", coefStr(x, sensor=1), "\n")
})

setMethod ("show", "SensorModel", function(object)
{
  cat(" Sensor Model (num ", numStr(object), "), beta ", beta(object), ", data model '", modelName(object), "'", "\n", sep='')
})

#----------------------------
# Get/Set Methods
#----------------------------

#' @rdname SensorModel-class
#' @aliases coeffNonneg,SensorModel-method
setMethod("coeffNonneg", "SensorModel", function(x) x@coeffNonneg)

#' @rdname get-methods
#' @aliases modelName,SensorModel-method
setMethod("modelName", "SensorModel", function(x) x@dataModel[[1]]$method)

#' @rdname get-methods
#' @aliases beta,SensorModel-method
setMethod("beta", "SensorModel", function(x) x@beta)

#----------------------------
# Noise Methods
#----------------------------

#' @name nsd<-
#' @aliases nsd<-,SensorModel-method
#' @rdname noise-methods
setReplaceMethod("nsd", "SensorModel", function(object, value) 
{
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

