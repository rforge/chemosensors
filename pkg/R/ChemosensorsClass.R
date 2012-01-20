# last revision: 9 Jan 2012

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
    return(c("SensorArrayModel", "ConcNoiseModel", "SensorNoiseModel", "SorptionModel"))    
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

setGeneric("num", function(x) standardGeneric("num"))
setGeneric("idx", function(x) standardGeneric("idx"))
setGeneric("gases", function(x) standardGeneric("gases"))
setGeneric("gind", function(x) standardGeneric("gind"))
setGeneric("ngases", function(x) standardGeneric("ngases"))
setGeneric("gnames", function(x) standardGeneric("gnames"))

setGeneric("nsensors", function(x) standardGeneric("nsensors"))

setGeneric("concUnits", function(x) standardGeneric("concUnits"))
setGeneric("concUnitsInt", function(x) standardGeneric("concUnitsInt"))

setGeneric("type", function(x) standardGeneric("type"))

setGeneric("coefnames", function(x) standardGeneric("coefnames"))
setGeneric("ncoef", function(x) standardGeneric("ncoef"))

# SensorModel
setGeneric("modelName", function(x) standardGeneric("modelName"))
setGeneric("coeffNonneg", function(x) standardGeneric("coeffNonneg"))

# ConcNoiseModel
setGeneric("csd", function(x) standardGeneric("csd"))
setGeneric("csd<-", function(object, value) standardGeneric("csd<-"))
setGeneric("logf", function(x) standardGeneric("logf"))

# SensorNoiseModel
setGeneric("ssd", function(x) standardGeneric("ssd"))
setGeneric("ssd<-", function(object, value) standardGeneric("ssd<-"))
setGeneric("noisef", function(x) standardGeneric("noisef"))

# SorptionModel
setGeneric("knum", function(x) standardGeneric("knum"))
setGeneric("concUnitsSorption", function(x) standardGeneric("concUnitsSorption"))

# Sensor 
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
