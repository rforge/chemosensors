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
#'
#' @name subClasses
#' @rdname chemosensors-package
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
# Plot Methods
#----------------------------

#' Method plot.
#' @name plot-methods
#' @rdname plot-methods
#' @exportMethod plot
setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

#' Method print.
#' @name class-methods
#' @rdname class-methods
#' @exportMethod print
setGeneric("print", function (x, ...) standardGeneric("print"))


#----------------------------
# Export Methods
#----------------------------

#' Method sdata.frame.
#' @name scenario-methods
#' @rdname scenario-methods
#' @exportMethod sdata.frame
setGeneric("sdata.frame", function(x, ...) standardGeneric("sdata.frame"))

#----------------------------
# Convert Methods
#----------------------------

#' Method getSensor.
#' @name SensorArray-class
#' @rdname SensorArray-class
#' @exportMethod getSensor
setGeneric("getSensor", function(object, index) standardGeneric("getSensor"))

#----------------------------
# Top-level Get/Set Methods
#----------------------------

#' Method alpha.
#' @name get-methods
#' @rdname get-methods
#' @exportMethod alpha
setGeneric("alpha", function(x) standardGeneric("alpha"))

#' Method beta.
#' @name get-methods
#' @rdname get-methods
#' @exportMethod beta
setGeneric("beta", function(x) standardGeneric("beta"))

#' Method modelName.
#' @name get-methods
#' @rdname get-methods
#' @exportMethod modelName
setGeneric("modelName", function(x) standardGeneric("modelName"))

#' Method tunit.
#' @name get-methods
#' @rdname get-methods
#' @exportMethod tunit
setGeneric("tunit", function(x) standardGeneric("tunit"))

#' Method enableSorption.
#' @name get-methods
#' @rdname get-methods
#' @exportMethod enableSorption
setGeneric("enableSorption", function(x) standardGeneric("enableSorption"))

#' Method enableDyn.
#' @name get-methods
#' @rdname get-methods
#' @exportMethod enableDyn
setGeneric("enableDyn", function(x) standardGeneric("enableDyn"))

#' Method enableDyn<-.
#' @name enableDyn<-
#' @rdname set-methods
#' @exportMethod enableDyn<-
setGeneric("enableDyn<-", function(object, value) standardGeneric("enableDyn<-"))

#----------------------------
# Get/Set Methods
#----------------------------

#' Method num.
#' @name get-methods
#' @rdname get-methods
#' @exportMethod num
setGeneric("num", function(x) standardGeneric("num"))

setGeneric("numStr", function(x) standardGeneric("numStr"))

#' Method idx.
#' @name get-methods
#' @rdname get-methods
#' @exportMethod idx
setGeneric("idx", function(x) standardGeneric("idx"))

#' Method gases.
#' @name get-methods
#' @rdname get-methods
#' @exportMethod gases
setGeneric("gases", function(x) standardGeneric("gases"))

#' Method gind.
#' @name get-methods
#' @rdname get-methods
#' @exportMethod gind
setGeneric("gind", function(x) standardGeneric("gind"))

#' Method ngases.
#' @name get-methods
#' @rdname get-methods
#' @exportMethod ngases
setGeneric("ngases", function(x) standardGeneric("ngases"))

#' Method gnames.
#' @name get-methods
#' @rdname get-methods
#' @exportMethod gnames
setGeneric("gnames", function(x) standardGeneric("gnames"))

#' Method coefficients.
#' @name get-methods
#' @rdname get-methods
#' @exportMethod coefficients
setGeneric("coefficients", function(object, ...) standardGeneric("coefficients"))

#' Method coef.
#' @name get-methods
#' @rdname get-methods
#' @exportMethod coef
setGeneric("coef", function(object, ...) standardGeneric("coef"))

setGeneric("coefStr", function(object, ...) standardGeneric("coefStr"))

#' Method nsensors.
#' @name get-methods
#' @rdname get-methods
#' @exportMethod nsensors
setGeneric("nsensors", function(x) standardGeneric("nsensors"))

#' Method snames.
#' @name get-methods
#' @rdname get-methods
#' @exportMethod snames
setGeneric("snames", function(x, ...) standardGeneric("snames"))

#' Method concUnits.
#' @name get-methods
#' @rdname get-methods
#' @exportMethod concUnits
setGeneric("concUnits", function(x) standardGeneric("concUnits"))

#' Method concUnits<-.
#' @name concUnits<-
#' @rdname set-methods
#' @exportMethod concUnits<-
setGeneric("concUnits<-", function(object, value) standardGeneric("concUnits<-"))

#' Method concUnitsInt.
#' @name get-methods
#' @rdname get-methods
#' @exportMethod concUnitsInt
setGeneric("concUnitsInt", function(x) standardGeneric("concUnitsInt"))

#' Method type.
#' @name get-methods
#' @rdname get-methods
#' @exportMethod type
setGeneric("type", function(x) standardGeneric("type"))

#' Method coefnames.
#' @name get-methods
#' @rdname get-methods
#' @exportMethod coefnames
setGeneric("coefnames", function(x) standardGeneric("coefnames"))

#' Method ncoef.
#' @name get-methods
#' @rdname get-methods
#' @exportMethod ncoef
setGeneric("ncoef", function(x) standardGeneric("ncoef"))

#----------------------------
# SensorModel
#----------------------------

#' Method coeffNonneg.
#' @name SensorModel-class
#' @rdname SensorModel-class
#' @exportMethod modelName
#' @exportMethod coeffNonneg
setGeneric("coeffNonneg", function(x) standardGeneric("coeffNonneg"))

#----------------------------
# ConcNoiseModel
#----------------------------

#' Method csd.
#' @name noise-methods
#' @rdname noise-methods
#' @exportMethod csd
setGeneric("csd", function(x) standardGeneric("csd"))

#' Method csd<-.
#' @name csd<-
#' @rdname noise-methods
#' @exportMethod csd<-
setGeneric("csd<-", function(object, value) standardGeneric("csd<-"))

#' Method logf.
#' @name ConcNoiseModel-class
#' @rdname ConcNoiseModel-class
#' @exportMethod logf
setGeneric("logf", function(x) standardGeneric("logf"))

#----------------------------
# SensorNoiseModel
#----------------------------

#' Method ssd.
#' @name noise-methods
#' @rdname noise-methods
#' @exportMethod ssd
setGeneric("ssd", function(x) standardGeneric("ssd"))

#' Method ssd<-.
#' @name ssd<-
#' @rdname noise-methods
#' @exportMethod ssd<-
setGeneric("ssd<-", function(object, value) standardGeneric("ssd<-"))

#' Method noisef.
#' @name SensorNoiseModel-class
#' @rdname SensorNoiseModel-class
#' @exportMethod noisef
setGeneric("noisef", function(x) standardGeneric("noisef"))

#----------------------------
# SorptionModel
#----------------------------

#' Method knum.
#' @name SorptionModel-class
#' @rdname SorptionModel-class
#' @exportMethod knum
setGeneric("knum", function(x) standardGeneric("knum"))

setGeneric("knumStr", function(x) standardGeneric("knumStr"))

#' Method concUnitsSorption.
#' @name SorptionModel-class
#' @rdname SorptionModel-class
#' @exportMethod concUnitsSorption
setGeneric("concUnitsSorption", function(x) standardGeneric("concUnitsSorption"))

#----------------------------
# DriftNoiseModel
#----------------------------

#' Method dsd.
#' @name noise-methods
#' @rdname noise-methods
#' @exportMethod dsd
setGeneric("dsd", function(x) standardGeneric("dsd"))

#' Method dsd<-.
#' @name dsd<-
#' @rdname noise-methods
#' @exportMethod dsd<-
setGeneric("dsd<-", function(object, value) standardGeneric("dsd<-"))

#' Method ndcomp.
#' @name DriftNoiseModel-class
#' @rdname DriftNoiseModel-class
#' @exportMethod ndcomp
setGeneric("ndcomp", function(x) standardGeneric("ndcomp"))

#' Method ndvar.
#' @name DriftNoiseModel-class
#' @rdname DriftNoiseModel-class
#' @exportMethod ndvar
setGeneric("ndvar", function(x) standardGeneric("ndvar"))

#' Method dspace.
#' @name DriftNoiseModel-class
#' @rdname DriftNoiseModel-class
#' @exportMethod dspace
setGeneric("dspace", function(x) standardGeneric("dspace"))

#----------------------------
# SensorDynamics
#----------------------------

#' Method nconst.
#' @name SensorDynamics-class
#' @rdname SensorDynamics-class
#' @exportMethod nconst
setGeneric("nconst", function(x) standardGeneric("nconst"))

#' Method trange.
#' @name SensorDynamics-class
#' @rdname SensorDynamics-class
#' @exportMethod trange
setGeneric("trange", function(x) standardGeneric("trange"))

#' Method tconst.
#' @name SensorDynamics-class
#' @rdname SensorDynamics-class
#' @exportMethod tconst
setGeneric("tconst", function(x) standardGeneric("tconst"))

#----------------------------
# ConcProfile
#----------------------------

#' Method nsamples.
#' @name scenario-methods
#' @rdname scenario-methods
#' @exportMethod nsamples
setGeneric("nsamples", function(x) standardGeneric("nsamples"))

#' Method cmatrix.
#' @name scenario-methods
#' @rdname scenario-methods
#' @exportMethod cmatrix
setGeneric("cmatrix", function(x, ...) standardGeneric("cmatrix"))

#' Method add<-.
#' @name add<-
#' @rdname scenario-methods
#' @exportMethod add<-
setGeneric("add<-", function(object, value) standardGeneric("add<-"))

#----------------------------
# Compute Methods
#----------------------------

#' Method computeAffinity.
#' @name compute-methods
#' @rdname compute-methods
#' @exportMethod computeAffinity
setGeneric("computeAffinity", function(object, method, norm = "none", ...) standardGeneric("computeAffinity"))

#----------------------------
# Model Methods
#----------------------------

setGeneric("concModel", function(object, conc, ...) standardGeneric("concModel"))

#' Method concMin.
#' @name model-methods
#' @rdname model-methods
#' @exportMethod concMin
setGeneric("concMin", function(object, ...) standardGeneric("concMin"))

#' Method concMax.
#' @name model-methods
#' @rdname model-methods
#' @exportMethod concMax
setGeneric("concMax", function(object, ...) standardGeneric("concMax"))

setGeneric("concNorm", function(object, conc, ...) standardGeneric("concNorm"))

setGeneric("concDenorm", function(object, conc, ...) standardGeneric("concDenorm"))

setGeneric("sdataModel", function(object, conc, ...) standardGeneric("sdataModel"))

#' Method concSample.
#' @name model-methods
#' @rdname model-methods
#' @exportMethod concSample
setGeneric("concSample", function(object, ...) standardGeneric("concSample"))

#' Method concSampleDyn.
#' @name model-methods
#' @rdname model-methods
#' @exportMethod concSampleDyn
setGeneric("concSampleDyn", function(object, ...) standardGeneric("concSampleDyn"))

#' Method predict.
#' @name model-methods
#' @rdname model-methods
#' @exportMethod predict
setGeneric("predict", function(object, ...) standardGeneric("predict"))

setGeneric("sdataSample", function(object, ...) standardGeneric("sdataSample"))

setGeneric("coefSample", function(object, ...) standardGeneric("coefSample"))

#' Method getConc.
#' @name model-methods
#' @rdname model-methods
#' @exportMethod getConc
setGeneric("getConc", function(object, ...) standardGeneric("getConc"))

setGeneric("extractConc", function(object, conc, set, scenario, n, cf, df, concUnits = "default", ...) standardGeneric("extractConc"))

setGeneric("extractSdata", function(object, conc, sdata, sf, df, concUnits = "default", ...) standardGeneric("extractSdata"))

#----------------------------
# Noise Methods
#----------------------------

setGeneric("scaledNoiseSd", function(object, ...) standardGeneric("scaledNoiseSd"))

#' Method nsd<-.
#' @name nsd<-
#' @rdname noise-methods
#' @exportMethod nsd<-
setGeneric("nsd<-", function(object, value) standardGeneric("nsd<-"))

#----------------------------
# Plot Methods
#----------------------------

#' Method plotPolar.
#' @name plot-methods
#' @rdname plot-methods
#' @exportMethod plotPolar
setGeneric("plotPolar", function(x, y, ...) standardGeneric("plotPolar")) 

#' Method plotPCA.
#' @name plot-methods
#' @rdname plot-methods
#' @exportMethod plotPCA
setGeneric("plotPCA", function(x, y, conc, sdata, set, scenario,
  feature = "transient", air = TRUE, 
  mod, center = TRUE, scale = TRUE, pc = 1:2, ...) standardGeneric("plotPCA")) 

#' Method plotBox.
#' @name plot-methods
#' @rdname plot-methods
#' @exportMethod plotBox
setGeneric("plotBox", function(x, y, conc, sdata, set, scenario, ...) standardGeneric("plotBox")) 

#' Method plotResponse.
#' @name plot-methods
#' @rdname plot-methods
#' @exportMethod plotResponse
setGeneric("plotResponse", function(x, y, ...) standardGeneric("plotResponse")) 

#' Method plotMixture.
#' @name plot-methods
#' @rdname plot-methods
#' @exportMethod plotMixture
setGeneric("plotMixture", function(x, y, ...) standardGeneric("plotMixture")) 

#' Method plotSignal.
#' @name plot-methods
#' @rdname plot-methods
#' @exportMethod plotSignal
setGeneric("plotSignal", function(x, y, ...) standardGeneric("plotSignal")) 

#' Method plotPolarGases.
#' @name plot-methods
#' @rdname plot-methods
#' @exportMethod plotPolarGases
setGeneric("plotPolarGases", function(x, y, ...) standardGeneric("plotPolarGases")) 

setGeneric("plotResponseOld", function(x, y, ...) standardGeneric("plotResponseOld")) 

#' Method plotTimeline.
#' @name plot-methods
#' @rdname plot-methods
#' @exportMethod plotTimeline
setGeneric("plotTimeline", function(x, y, ...) standardGeneric("plotTimeline")) 

#' Method plotAffinity.
#' @name plot-methods
#' @rdname plot-methods
#' @exportMethod plotAffinity
setGeneric("plotAffinity", function(x, y, ...) standardGeneric("plotAffinity")) 

#' Method plotAffinityMap.
#' @name plot-methods
#' @rdname plot-methods
#' @exportMethod plotAffinityMap
setGeneric("plotAffinityMap", function(x, y, ...) standardGeneric("plotAffinityMap")) 

setGeneric("plotAffinitySpaceOld", function(x, y, conc, sdata, ...) standardGeneric("plotAffinitySpaceOld")) 

#' Method plotAffinitySpace.
#' @name plot-methods
#' @rdname plot-methods
#' @exportMethod plotAffinitySpace
setGeneric("plotAffinitySpace", function(x, y, conc, sdata, ...) standardGeneric("plotAffinitySpace")) 

#' Method ccol.
#' @name plot-methods
#' @rdname plot-methods
#' @exportMethod ccol
setGeneric("ccol", function(object, ...) standardGeneric("ccol")) 

#' Method gcol.
#' @name plot-methods
#' @rdname plot-methods
#' @exportMethod gcol
setGeneric("gcol", function(object, ...) standardGeneric("gcol")) 

#' Method scol.
#' @name plot-methods
#' @rdname plot-methods
#' @exportMethod scol
setGeneric("scol", function(object, ...) standardGeneric("scol")) 

#' Method mapcol.
#' @name plot-methods
#' @rdname plot-methods
#' @exportMethod mapcol
setGeneric("mapcol", function(object, ...) standardGeneric("mapcol")) 

#----------------------------
# Other Methods
#----------------------------

# Method affinity.
#
# Method \code{affinity} evalates the affinity of sensors.
#
# The affinity coefficients are computed per analyte,
# and express strength of a given sensor in response to the given analyte.
# We use the definition of receptor/ligand binding affinity from biochemistry here.
# The analog of the dissociation constant \code{Kd} ic computed from the sensibility curve,
# and the affinicty coefficient is given by formula \code{Ka = 1/Kd}.
# @name affinity
# @rdname affinity
# @seealso \code{\link{SensorArray}}
# @example inst/examples/affinity-method.R

#' Method affinity.
#' @name SensorArray-class
#' @rdname SensorArray-class
#' @exportMethod affinity
setGeneric("affinity", function(object, ...) standardGeneric("affinity")) 

#----------------------------
# Conc Methods
#----------------------------

#' Method checkConc.
#' @name scenario-methods
#' @rdname scenario-methods
#' @exportMethod checkConc
setGeneric("checkConc", function(object, conc, ...) standardGeneric("checkConc"))

#' Method sdata2feature.
#' @name scenario-methods
#' @rdname scenario-methods
#' @exportMethod sdata2feature
setGeneric("sdata2feature", function(object, conc, sdata, ...) standardGeneric("sdata2feature"))

#' Method sdata2df.
#' @name scenario-methods
#' @rdname scenario-methods
#' @exportMethod sdata2df
setGeneric("sdata2df", function(object, sdata, ...) standardGeneric("sdata2df"))

#' Method conc2df.
#' @name scenario-methods
#' @rdname scenario-methods
#' @exportMethod conc2df
setGeneric("conc2df", function(object, conc, ...) standardGeneric("conc2df"))

#' Method conc2lab.
#' @name scenario-methods
#' @rdname scenario-methods
#' @exportMethod conc2lab
setGeneric("conc2lab", function(object, ...) standardGeneric("conc2lab"))

#' Method conc2glab.
#' @name scenario-methods
#' @rdname scenario-methods
#' @exportMethod conc2glab
setGeneric("conc2glab", function(object, ...) standardGeneric("conc2glab"))

#' Method conc2col.
#' @name scenario-methods
#' @rdname scenario-methods
#' @exportMethod conc2col
setGeneric("conc2col", function(object, ...) standardGeneric("conc2col"))

setGeneric("conc2gcol", function(object, ...) standardGeneric("conc2gcol"))

#' Method getTPoint.
#' @name scenario-methods
#' @rdname scenario-methods
#' @exportMethod getTPoint
setGeneric("getTPoint", function(object, conc, ...) standardGeneric("getTPoint")) 

#' Method conc2tpoint.
#' @name scenario-methods
#' @rdname scenario-methods
#' @exportMethod conc2tpoint
setGeneric("conc2tpoint", function(object, ...) standardGeneric("conc2tpoint"))

#' Method lab2df.
#' @name scenario-methods
#' @rdname scenario-methods
#' @exportMethod lab2df
setGeneric("lab2df", function(object, ...) standardGeneric("lab2df"))

#' Method label2df.
#' @name scenario-methods
#' @rdname scenario-methods
#' @exportMethod label2df
setGeneric("label2df", function(object, ...) standardGeneric("label2df"))

#' Method set2lab.
#' @name scenario-methods
#' @rdname scenario-methods
#' @exportMethod set2lab
setGeneric("set2lab", function(object, ...) standardGeneric("set2lab"))

#----------------------------
# Sdata Methods
#----------------------------
#' @exportMethod sdata2pulse
setGeneric("sdata2pulse", function(object, ...) standardGeneric("sdata2pulse"))
