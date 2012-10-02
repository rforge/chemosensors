# last revision: 12 Jan 2012

#' @include ChemosensorsClassMethods.R
NULL

#----------------------------
# Support Class defintion
#----------------------------


#----------------------------
# Class defintion
#----------------------------

#' The function \code{validObject} for class \code{\link{Scenario}}.
#'
#' @param object A \code{\link{Scenario}} object
#' @return TRUE if the object is valid. Otherwise, a character error string.
#' @name validScenario
#' @rdname int-validScenario
validScenario <- function(object)
{
  return(TRUE)
}

#' Class Scenario.
#'
#' Class \code{Scenario} represents a concentration profile to build a concentration matrix.
#'
#' Slots of the class:
#' \tabular{rl}{
#'   \code{tunit} \tab Time length of a single gas pulse.
#'     The unity value means experiments with steady-state sensor signals. The default value is \code{1}. \cr
#'   \code{gases} \tab Gas indices. \cr
#'   \code{ngases} \tab The number of gases. \cr
#'   \code{gnames} \tab Names of gases. \cr
#'   \code{concUnits} \tab Concentration units external to the model, values given in an input concentration matrix. \cr
#'   \code{concUnitsInt} \tab Concentration units internal to the model. \cr
#'   \code{df} \tab Data frame of a concentration profile with the columns:
#'     names of gases (\code{gnames}), \code{time}, \code{set}, \code{lab} and \code{tpoint}. \cr
#' }
#'
#' Methods of the class:
#' \tabular{rl}{
#'   \code{add<-} \tab A replacement method to add gas pulses to the object. \cr
#'   \code{getConc} \tab Get the concentration matrix of the object. \cr
#' }
#' 
#' The \code{plot} method has one type (parameter \code{y}):
#' \tabular{rl}{
#'   \code{time} \tab Shows the concentration of gases over time. \cr
#' }
#' @name Scenario
#' @rdname www-Scenario
#' @keywords Scenario-class
#' @example inst/examples/Scenario-class.R
#' @exportClass Scenario
setClass(Class="Scenario", 
  representation=representation(
    # class-specific slots
    df = "data.frame", # colnames: cmatrix, time, set, lab
    tunit = "numeric", 
    # class-common slots
    gases = "numeric", gind = "numeric", ngases = "numeric", gnames="character", 
    concUnits="character", concUnitsInt="character"),  
  validity=validScenario
)

#----------------------------
# Print/Show Methods
#----------------------------

#' @exportMethod print
setMethod ("print", "Scenario", function(x, ...)
{
  cat(" Scenario\n")
  cat(" -", nsamples(x), "samples\n")  
  cat(" -", ngases(x), "gases", paste(gnames(x), collapse=", "), "\n")
  cat(" - tunit:", tunit(x), "\n")
})

#' @exportMethod show
setMethod ("show", "Scenario", function(object)
{
  cat(" Scenario (", nsamples(object), " samples x ", ngases(object), " gases ", paste(gnames(object), collapse=", "), ")\n", sep='')
})


#----------------------------
# Plot Methods
#----------------------------

#----------------------------
# Get/Set Methods
#----------------------------

setMethod("tunit", "Scenario", function(x) x@tunit)
setMethod("nsamples", "Scenario", function(x) nrow(x@df))
setMethod("cmatrix", "Scenario", function(x) subset(x@df, select=gnames(x)))

#----------------------------
# Model Methods
#----------------------------

#----------------------------
# Predict methods
#----------------------------


