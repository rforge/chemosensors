# last revision: 12 Jan 2012

#' @include ChemosensorsClassMethods.R
NULL

#----------------------------
# Support Class defintion
#----------------------------


#----------------------------
# Class defintion
#----------------------------

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
#' @name Scenario-class
#' @rdname Scenario-class
#' @example inst/examples/Scenario-class.R
#' @exportClass Scenario
setClass(Class="Scenario", 
  representation=representation(
    name = "character",
    # class-specific slots
    T = "character", nT = "numeric", V = "character", nV = "numeric",
    randomize = "logical",
    tunit = "numeric", 
    # class-common slots
    gases = "numeric", gind = "numeric", ngases = "numeric", gnames="character", 
    concUnits = "character", concUnitsInt="character",
    seed = "numeric"),  
  validity=validScenario
)

#----------------------------
# Print/Show Methods
#----------------------------

#' @rdname class-methods
#' @aliases print,Scenario-method
setMethod ("print", "Scenario", function(x, ...)
{
  cat(" Scenario")
  if(x@name != "undefined") cat(" `", x@name,"`\n", sep = '')
  else cat("\n")
  cat(" -", nsamples(x), "samples\n")  
  cat(" -", ngases(x), "gases", paste(gnames(x), collapse=", "), "\n")
  cat(" - tunit:", tunit(x), "\n")
})

setMethod ("show", "Scenario", function(object)
{
  # local functions
  set_to_str <- function(set, n)
  {
    if(length(n) == 0 | length(n) == 1 & n[1] == 0) { return("empty") }
    
    # dummy assignment to get rid of errors from `R CMD check` (no visible binding for global variable ‘label’)
    label <- NULL
    
    df <- data.frame(label = rep(set, n)) # training frame
    sf <- ddply(df, "label", summarise, num = length(label))
    paste(paste(sf$label, ' (', sf$num, ')', sep = ''), collapse = ', ') # looks like [1] "A (2), B (3)"
  }

  randomize.str <- ifelse(object@randomize, "TRUE", "FALSE")

  # print header string
  cat(" Scenario")
  if(object@name != "undefined") cat(" `", object@name,"`", sep = '')
  cat(" of ", nsamples(object), " samples, tunit ", object@tunit,", randomize ", randomize.str, "\n", sep = '')

  # print gases
  cat(" - gases ", paste(gnames(object), collapse=", "), "\n", sep='')
    
  # print T/V stat
  cat(" - Training Set:", set_to_str(object@T, object@nT), "\n")
  cat(" - Validation Set:", set_to_str(object@V, object@nV), "\n")  

})


#----------------------------
# Plot Methods
#----------------------------

#----------------------------
# Get/Set Methods
#----------------------------

#' @rdname get-methods
#' @aliases tunit,Scenario-method
setMethod("tunit", "Scenario", function(x) x@tunit)

#' @rdname scenario-methods
#' @aliases nsamples,Scenario-method
setMethod("nsamples", "Scenario", function(x) sum(x@nT) + sum(x@nV))

#' @rdname scenario-methods
#' @aliases cmatrix,Scenario-method
setMethod("cmatrix", "Scenario", function(x, ...) subset(sdata.frame(x, ...), select = gnames(x)))

#----------------------------
# Model Methods
#----------------------------

#----------------------------
# Predict methods
#----------------------------


