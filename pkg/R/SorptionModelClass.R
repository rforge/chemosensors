# last revision: 16 Jan 2012

#' @include ChemosensorsClass.R
NULL

#----------------------------
# Class defintion
#----------------------------

#' The function \code{validObject} for class \code{\link{SorptionModel}}.
#'
#' @param object A \code{\link{SorptionModel}} object
#' @return TRUE if the object is valid. Otherwise, a character error string.
#' @name validSorptionModel
#' @rdname int-validSorptionModel
validSorptionModel <- function(object)
{
  return(TRUE)
}

#' Class SorptionModel.
#'
#' Class \code{\link{SorptionModel}} generates a sensor noise.
#'
#' Slots of the class:
#' \tabular{rl}{
#'   \code{knum} \tab Sensor number that encodes a UNIMAN sorption profile. \cr
#'   \code{sorptionModel} \tab List of model parameters 'K' and 'Q'. \cr
#' }
#' @name SorptionModel
#' @rdname www-SorptionModel
#' @keywords SorptionModel-class
#' @example R/example/SorptionModel-class.R
#' @exportClass SorptionModel
setClass(Class="SorptionModel", 
  representation=representation(
    # class-specific slots
    knum = "numeric",
    # class-common slots
    idx = "numeric", 
    gases = "numeric", ngases = "numeric", gind = "numeric", gnames="character", 
    concUnits="character", concUnitsSorption="character",
    # class-specific slots
    srdata = "array", sorptionModel = "list"),  
  validity=validSorptionModel
)


#----------------------------
# Print/Show Methods
#----------------------------

#' @exportMethod print
setMethod ("print","SorptionModel", function(x, ...)
{
  cat(" Sorption Model\n")
  cat(" - knum:", paste(knum(x), collapse=", "), "\n")
  cat(" -", ngases(x), "gases", paste(gnames(x), collapse=", "), "\n")
})

#' @exportMethod show
setMethod ("show","SorptionModel", function(object)
{
  cat(" Sorption Model (knum ", paste(knum(object), collapse=", "), "), (", ngases(object), " gases)", "\n", sep='')
})


#----------------------------
# Plot Methods
#----------------------------

#----------------------------
# Get/Set Methods
#----------------------------

setMethod("knum", "SorptionModel", function(x) x@knum)
setMethod("concUnitsSorption", "SorptionModel", function(x) x@concUnitsSorption)

setMethod("nsensors", "SorptionModel", function(x) length(x@knum))

#----------------------------
# Model Methods
#----------------------------

#----------------------------
# Predict methods
#----------------------------

