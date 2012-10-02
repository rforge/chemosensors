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
  if(class == "SensorModel")
    return(c("SensorDynamics"))
  else if(class == "Sensor")
    return(c("SensorModel", "ConcNoiseModel", "SensorNoiseModel", "SorptionModel"))
  else if(class == "DriftNoiseModel")
    return(c("DriftCommonModel"))
  else if(class == "SensorNoiseModel")
    return(c("DriftCommonModel"))
  else if(class == "SensorArray")
    return(c("SensorModel", "SorptionModel", "ConcNoiseModel", "SensorNoiseModel", "DriftNoiseModel"))
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
#' @exportMethod numStr
setGeneric("numStr", function(x) standardGeneric("numStr"))
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

#' @exportMethod coefficients
setGeneric("coefficients", function(object, ...) standardGeneric("coefficients"))
#' @exportMethod coef
setGeneric("coef", function(object, ...) standardGeneric("coef"))
setGeneric("coefStr", function(object, ...) standardGeneric("coefStr"))


#' @exportMethod nsensors
setGeneric("nsensors", function(x) standardGeneric("nsensors"))

#' @exportMethod concUnits
setGeneric("concUnits", function(x) standardGeneric("concUnits"))
#' @exportMethod concUnits<-
setGeneric("concUnits<-", function(object, value) standardGeneric("concUnits<-"))
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
#' @exportMethod knumStr
setGeneric("knumStr", function(x) standardGeneric("knumStr"))
#' @exportMethod concUnitsSorption
setGeneric("concUnitsSorption", function(x) standardGeneric("concUnitsSorption"))

#' @exportMethod alpha
setGeneric("alpha", function(x) standardGeneric("alpha"))
#' @exportMethod beta
setGeneric("beta", function(x) standardGeneric("beta"))

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

# ConcProfile
#' @exportMethod tunit
setGeneric("tunit", function(x) standardGeneric("tunit"))
#' @exportMethod nsamples
setGeneric("nsamples", function(x) standardGeneric("nsamples"))
#' @exportMethod cmatrix
setGeneric("cmatrix", function(x) standardGeneric("cmatrix"))
#' @exportMethod add<-
setGeneric("add<-", function(object, ...) standardGeneric("add<-"))

# SensorDynamics
#' @exportMethod nconst
setGeneric("nconst", function(x) standardGeneric("nconst"))
#' @exportMethod trange
setGeneric("trange", function(x) standardGeneric("trange"))
#' @exportMethod tconst
setGeneric("tconst", function(x) standardGeneric("tconst"))
#' @exportMethod enableDyn
setGeneric("enableDyn", function(x) standardGeneric("enableDyn"))
#' @exportMethod enableDyn<-
setGeneric("enableDyn<-", function(object, value) standardGeneric("enableDyn<-"))

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

#' @exportMethod concSampleDyn
setGeneric("concSampleDyn", function(object, ...) standardGeneric("concSampleDyn"))

#' @exportMethod predict
setGeneric("predict", function(object, ...) standardGeneric("predict"))

#' @exportMethod sdataSample
setGeneric("sdataSample", function(object, ...) standardGeneric("sdataSample"))

#' @exportMethod coefSample
setGeneric("coefSample", function(object, ...) standardGeneric("coefSample"))

#' @exportMethod getConc
setGeneric("getConc", function(object, ...) standardGeneric("getConc"))


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

#' @exportMethod plotPolar
setGeneric("plotPolar", function(x, y, ...) standardGeneric("plotPolar")) 

#' @exportMethod plotPolarGases
setGeneric("plotPolarGases", function(x, y, ...) standardGeneric("plotPolarGases")) 

#' @exportMethod plotResponse
setGeneric("plotResponse", function(x, y, ...) standardGeneric("plotResponse")) 

#' @exportMethod plotPCA
setGeneric("plotPCA", function(x, y, ...) standardGeneric("plotPCA")) 

#' @exportMethod plotTimeline
setGeneric("plotTimeline", function(x, y, ...) standardGeneric("plotTimeline")) 

#' @exportMethod plotAffinity
setGeneric("plotAffinity", function(x, y, ...) standardGeneric("plotAffinity")) 

#' @exportMethod plotAffinity
setGeneric("plotAffinity", function(x, y, ...) standardGeneric("plotAffinity")) 

#' @exportMethod plotAffinityMap
setGeneric("plotAffinityMap", function(x, y, ...) standardGeneric("plotAffinityMap")) 

#' @exportMethod plotAffinitySpaceOld
setGeneric("plotAffinitySpaceOld", function(x, y, conc, sdata, ...) standardGeneric("plotAffinitySpaceOld")) 

#' @exportMethod plotAffinitySpace
setGeneric("plotAffinitySpace", function(x, y, conc, sdata, ...) standardGeneric("plotAffinitySpace")) 

#' @exportMethod plotMixture
setGeneric("plotMixture", function(x, y, ...) standardGeneric("plotMixture")) 

#' @exportMethod ccol
setGeneric("ccol", function(object, ...) standardGeneric("ccol")) 

#' @exportMethod gcol
setGeneric("gcol", function(object, ...) standardGeneric("gcol")) 

#' @exportMethod scol
setGeneric("scol", function(object, ...) standardGeneric("scol")) 

#' @exportMethod mapcol
setGeneric("mapcol", function(object, ...) standardGeneric("mapcol")) 

#----------------------------
# Other Methods
#----------------------------

#' Method affinity.
#'
#' Method \code{affinity} evalates the affinity of sensors.
#'
#' The affinity coefficients are computed per analyte,
#' and express strength of a given sensor in response to the given analyte.
#' We use the definition of receptor/ligand binding affinity from biochemistry here.
#' The analog of the dissociation constant \code{Kd} ic computed from the sensibility curve,
#' and the affinicty coefficient is given by formula \code{Ka = 1/Kd}.
#' @name affinity
#' @rdname www-affinity
#' @seealso \code{\link{SensorArray}}
#' @example inst/examples/affinity-method.R
#' @exportMethod affinity
setGeneric("affinity", function(object, ...) standardGeneric("affinity")) 

#----------------------------
# Conc Methods
#----------------------------

#' @exportMethod conc2df
setGeneric("conc2df", function(object, conc, ...) standardGeneric("conc2df"))

#' @exportMethod conc2lab
setGeneric("conc2lab", function(object, ...) standardGeneric("conc2lab"))

#' @exportMethod conc2glab
setGeneric("conc2glab", function(object, ...) standardGeneric("conc2glab"))

#' @exportMethod conc2col
setGeneric("conc2col", function(object, ...) standardGeneric("conc2col"))

#' @exportMethod conc2gcol
setGeneric("conc2gcol", function(object, ...) standardGeneric("conc2gcol"))

#' @exportMethod conc2tpoint
setGeneric("conc2tpoint", function(object, ...) standardGeneric("conc2tpoint"))

#----------------------------
# Sdata Methods
#----------------------------
#' @exportMethod sdata2pulse
setGeneric("sdata2pulse", function(object, ...) standardGeneric("sdata2pulse"))
