# last revision: 9 Jan 2012

#' @include support.lib.R
NULL

#----------------------------
# Sub Classes
#----------------------------

#' Support function to get sub-classes.
#'
#' @param class Class name.
#' @return Character vector of sub-classes.
#' @name subClasses
#' @rdname int-subClasses
#' @export
subClasses <- function(class)
{
  if(class == "Sensor")
    #return(c("SensorModel", "SensorNoiseModel", "SorptionModel", "SensorDynamics"))
    return(c("SensorModel", "ConcNoiseModel", "SensorNoiseModel", "SorptionModel"))
  else if(class == "DriftNoiseModel")
    return(c("DriftCommonModel"))
  else if(class == "SensorNoiseModel")
    return(c("DriftCommonModel"))
  else if(class == "SensorArray")
    return(c("SensorArrayModel", "ConcNoiseModel", "SensorNoiseModel", "SorptionModel", "DriftNoiseModel"))
  else if(class == "SDataBox")
    return(c("Scenario", "SensorArray"))    
  else
    stop("Error in subClasses in ChemoesensorsClassMethods.R: 'class' is unknown.")
}

#----------------------------
# Parameters
#----------------------------

#----------------------------
# Defaults
#----------------------------

#----------------------------
# Get/Set Methods
#----------------------------
#' @exportMethod num
setGeneric("num", function(x) standardGeneric("num"))
#' @exportMethod idx
setGeneric("idx", function(x) standardGeneric("idx"))
#' @exportMethod gases
setGeneric("gases", function(x) standardGeneric("gases"))
#' @exportMethod gind
setGeneric("gind", function(x) standardGeneric("gind"))
#' @exportMethod ngases
setGeneric("ngases", function(x) standardGeneric("ngases"))
#' @exportMethod gnames
setGeneric("gnames", function(x) standardGeneric("gnames"))

#' @exportMethod nsensors
setGeneric("nsensors", function(x) standardGeneric("nsensors"))

#' @exportMethod concUnits
setGeneric("concUnits", function(x) standardGeneric("concUnits"))
#' @exportMethod concUnitsInt
setGeneric("concUnitsInt", function(x) standardGeneric("concUnitsInt"))

#' @exportMethod type
setGeneric("type", function(x) standardGeneric("type"))

#' @exportMethod coefnames
setGeneric("coefnames", function(x) standardGeneric("coefnames"))
#' @exportMethod ncoef
setGeneric("ncoef", function(x) standardGeneric("ncoef"))

# SensorModel
#' @exportMethod modelName
setGeneric("modelName", function(x) standardGeneric("modelName"))
#' @exportMethod coeffNonneg
setGeneric("coeffNonneg", function(x) standardGeneric("coeffNonneg"))

# ConcNoiseModel
#' @exportMethod csd
setGeneric("csd", function(x) standardGeneric("csd"))
#' @exportMethod csd<-
setGeneric("csd<-", function(object, value) standardGeneric("csd<-"))
#' @exportMethod logf
setGeneric("logf", function(x) standardGeneric("logf"))

# SensorNoiseModel
#' @exportMethod ssd
setGeneric("ssd", function(x) standardGeneric("ssd"))
#' @exportMethod ssd<-
setGeneric("ssd<-", function(object, value) standardGeneric("ssd<-"))
#' @exportMethod noisef
setGeneric("noisef", function(x) standardGeneric("noisef"))

# SorptionModel
#' @exportMethod knum
setGeneric("knum", function(x) standardGeneric("knum"))
#' @exportMethod concUnitsSorption
setGeneric("concUnitsSorption", function(x) standardGeneric("concUnitsSorption"))

# DriftNoiseModel
#' @exportMethod dsd
setGeneric("dsd", function(x) standardGeneric("dsd"))
#' @exportMethod dsd<-
setGeneric("dsd<-", function(object, value) standardGeneric("dsd<-"))
#' @exportMethod ndcomp
setGeneric("ndcomp", function(x) standardGeneric("ndcomp"))
#' @exportMethod ndvar
setGeneric("ndvar", function(x) standardGeneric("ndvar"))
#' @exportMethod dspace
setGeneric("dspace", function(x) standardGeneric("dspace"))

# Sensor 
#' @exportMethod enableSorption
setGeneric("enableSorption", function(x) standardGeneric("enableSorption"))

#----------------------------
# Model Methods
#----------------------------

#' @exportMethod concModel
setGeneric("concModel", function(object, conc, ...) standardGeneric("concModel"))

#' @exportMethod concMin
setGeneric("concMin", function(object, ...) standardGeneric("concMin"))

#' @exportMethod concMax
setGeneric("concMax", function(object, ...) standardGeneric("concMax"))

#' @exportMethod concNorm
setGeneric("concNorm", function(object, conc, ...) standardGeneric("concNorm"))

#' @exportMethod concDenorm
setGeneric("concDenorm", function(object, conc, ...) standardGeneric("concDenorm"))

#' @exportMethod sdataModel
setGeneric("sdataModel", function(object, conc, ...) standardGeneric("sdataModel"))

#' @exportMethod concSample
setGeneric("concSample", function(object, ...) standardGeneric("concSample"))

#' @exportMethod coefSample
setGeneric("coefSample", function(object, ...) standardGeneric("coefSample"))

#----------------------------
# Noise Methods
#----------------------------

#' @exportMethod scaledNoiseSd
setGeneric("scaledNoiseSd", function(object, ...) standardGeneric("scaledNoiseSd"))
#' @exportMethod nsd<-
setGeneric("nsd<-", function(object, value) standardGeneric("nsd<-"))

#----------------------------
# Plot Methods
#----------------------------

#' @exportMethod plotResponse
setGeneric("plotResponse", function(x, y, ...) standardGeneric("plotResponse")) 

#' @exportMethod ccol
setGeneric("ccol", function(object, ...) standardGeneric("ccol")) 

#' @exportMethod gcol
setGeneric("gcol", function(object, ...) standardGeneric("gcol")) 
