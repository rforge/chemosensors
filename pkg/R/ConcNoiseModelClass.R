# last revision: 12 Jan 2012

#' @include ChemosensorsClassMethods.R
NULL

#----------------------------
# Support Class defintion
#----------------------------


#----------------------------
# Class defintion
#----------------------------

#' The function \code{validObject} for class \code{\link{ConcNoiseModel}}.
#'
#' @param object A \code{\link{ConcNoiseModel}} object
#' @return TRUE if the object is valid. Otherwise, a character error string.
#' @name validConcNoiseModel
#' @rdname int-validConcNoiseModel
validConcNoiseModel <- function(object)
{
  if(sum(csd(object) < 0)) return("'csd' is negative")    
  
  return(TRUE)
}

#' Class ConcNoiseModel.
#'
#' Class \code{\link{ConcNoiseModel}} generates a concentration noise.
#'
#' Slots of the class:
#' \tabular{rl}{
#'   \code{csd} \tab Parameter of standard deviation used to generate the noise.
#'     The deault value is 0.1. \cr
#'   \code{cntype} \tab Noise type. One type is implemented, 'logconc'. \cr
#'   \code{cnlogf} \tab Scaling factor for log-terms used to generate the noise.
#'     The default value is c(1, 1, 2). \cr
#' }
#' @name ConcNoiseModel
#' @rdname www-ConcNoiseModel
#' @keywords ConcNoiseModel-class
#@example R/example/ConcNoiseModel-class.R
#' @exportClass ConcNoiseModel
setClass(Class="ConcNoiseModel", 
  representation=representation(
    # class-specific slots
    csd = "numeric",
    # class-common slots
    gases = "numeric", gind = "numeric", ngases = "numeric", gnames="character", 
    concUnits="character", concUnitsInt="character",
    cntype = "character", cnlogf = "numeric"),  
  validity=validConcNoiseModel
)


#----------------------------
# Print/Show Methods
#----------------------------

#' @exportMethod print
setMethod ("print","ConcNoiseModel", function(x, ...)
{
  cat(" Concentration Noise Model\n")
  cat(" -", ngases(x), "gases", paste(gnames(x), collapse=", "), "\n")
  cat(" - csd:", paste(csd(x), collapse=", "), "\n")
  cat(" - noise type:", type(x), "\n")
  if(type(x) == "logconc") 
    cat(" - log-factor:", paste(logf(x), collapse=", "), "\n")    
})

#' @exportMethod show
setMethod ("show","ConcNoiseModel", function(object)
{
  cat(" Concentration Noise Model (csd ", paste(csd(object), collapse=", "), "), ", "noise type '", type(object), "'", "\n", sep='')
})


#----------------------------
# Plot Methods
#----------------------------

#----------------------------
# Get/Set Methods
#----------------------------

setMethod("csd", "ConcNoiseModel", function(x) x@csd)
setMethod("type", "ConcNoiseModel", function(x) x@cntype)
setMethod("logf", "ConcNoiseModel", function(x) x@cnlogf)

setReplaceMethod("csd", "ConcNoiseModel", function(object, value) 
{
  object@csd <- value
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
setReplaceMethod("nsd", "ConcNoiseModel", function(object, value) 
{
  csd(object) <- value
  validObject(object)
  return (object)
})

