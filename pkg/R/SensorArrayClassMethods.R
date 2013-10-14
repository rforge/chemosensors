# last revision: 18 Jan 2012

#' @include SensorArrayClass.R
NULL

#----------------------------
# Class constructor
#----------------------------

#' Function to get default constructor parameters of class \code{\link{SensorArray}}.
#' @rdname SensorArray-class
#' @aliases defaultParSensorArray
#' @return List of the default parameters.
#' @export
defaultParSensorArray <- function()
{
  par <- list(type = "polymeric",
    enableSorption = TRUE, 
    nsensors = 2, num = 1:2, gases=1:3, gnames=LETTERS[1:3], 
    concUnits="perc", concUnitsInt=defaultConcUnitsInt(), concUnitsSorption = defaultConcUnitsSorption(),
    # Sorption Model
    knum = 1:2,
    # Sensor Model
    model = defaultDataModel(),
    datasetSensorModel=defaultDataSensorModel(),  datasetDistr=defaultDataDistr(), 
    datasetSensorNoiseModel=defaultDataSensorNoiseModel(), pck=defaultDataPackage(), 
    Conc0=NULL, Conc=NULL, dat=NULL, sndata=NULL, coefsd=NULL)
  
  return(par)
}

#' Constructor method of SensorArray Class.
#'
#' @name SensorArray
#' @rdname SensorArray-class
setMethod("initialize", "SensorArray", function(.Object,
  # specific for class SensorArray
  type = "character",
  enableSorption = "logical",
  # common for sub-classes
  nsensors="numeric", num="numeric", gases="numeric", gnames="character", 
  concUnits="character", concUnitsInt="character", concUnitsIntSorption="character", 
  # specific for Sorption Model
  knum="numeric",
  # specific for SensorArray Model
  model = "character",
  datasetSensorModel = "character", datasetDistr = "character", datasetSensorNoiseModel = "character", pck = "character", 
  Conc0, Conc, dat, sndata, coefsd, ...)  
{ 
  # missing
  def.par <- defaultParSensorArray()

  if(missing(type)) type <- def.par$type
  if(missing(enableSorption)) enableSorption <- def.par$enableSorption
      
  # missing 'nsensors', 'num', 'knum'
  if(missing(num)) {
    if(missing(nsensors)) num <- def.par$num
    else {
      num <- seq(1, min(nsensors, 17))
      nrep <- as.integer(nsensors / length(num)) + 1
      num <- rep(num, nrep)[1:nsensors]
    }
  }    
  else {
    if(!missing(nsensors)) {
      nrep <- as.integer(nsensors / length(num)) + 1    
      num <- rep(num, nrep)[1:nsensors]  
    }
  }
  nsensors <- length(num)
  if(missing(knum)) knum <- num
  
  if(missing(gases)) gases <- def.par$gases
  if(missing(gnames)) gnames <- def.par$gnames
  if(missing(concUnits)) concUnits <- def.par$concUnits
  if(missing(concUnitsInt)) concUnitsInt <- def.par$concUnitsInt
  if(missing(concUnitsIntSorption)) concUnitsIntSorption <- def.par$concUnitsIntSorption
  
  if(missing(model)) model <- def.par$model
  if(missing(datasetSensorModel)) datasetSensorModel <- def.par$datasetSensorModel
  if(missing(datasetSensorNoiseModel)) datasetSensorNoiseModel <- def.par$datasetSensorNoiseModel  
  if(missing(datasetDistr)) datasetDistr <- def.par$datasetDistr    
  if(missing(pck)) pck <- def.par$pck
  if(missing(Conc0)) Conc0 <- def.par$Conc0
  if(missing(Conc)) Conc <- def.par$Conc
  if(missing(dat)) dat <- def.par$dat
  if(missing(sndata)) sndata <- def.par$sndata
  if(missing(coefsd)) coefsd <- def.par$coefsd
    
  # 'idx'
  idx <- 1:length(num)
        
  # check
  if(length(num) != length(knum)) 
    stop("Error in 'SensorArray::initialize': 'length(num) != length(knum)'.")    

  # load data (1) 'datasetSensorModel'
  if(is.null(Conc0) | is.null(dat)) {
    #data(list=datasetSensorModel, package=pck, envir=environment()) # -> 'C', 'dat', 'dat.corrected'
    #if(!(exists("C") & exists("dat") & exists("dat.corrected")))
    #  stop("Error in SensorArray::initialize: 'datasetSensorModel' is not loaded; variables 'C', 'dat' and 'dat.corrected' not found.")    
    
    #if(!exists(datasetSensorModel)) # datasetSensorModel supposed to be `UNIMANshort`
    #  stop("Error in SensorArray::initialize: dataset", datasetSensorModel, "is not loaded.")    
    #eval(parse(text = paste("mdat <-", datasetSensorModel)))

    mdat <- loadUNIMANdata(datasetSensorModel)
    
    C <- mdat$C
    dat <- mdat$dat
    dat.corrected <- mdat$dat.corrected
    
    Conc0 <- C
    # dat == dat
  }
  
  if(is.null(Conc)) Conc <- C
  if(sum(dim(Conc0) == dim(Conc)) != 2)
    stop("Error in SensorArray::initialize: dimension of 'Conc0' and 'Conc' are different.")    

  # load data (2) 'datasetSensorNoiseModel'    
  if(is.null(sndata)) {
    #data(list=datasetSensorNoiseModel, package=pck, envir=environment()) # -> 'Bsd'
    #if(!(exists("Bsd")))
    #  stop("Error in SensorArray::initialize: 'datasetSensorNoiseModel' is not loaded; variable 'Bsd' is not found.")    
    
    #if(!exists(datasetSensorNoiseModel)) # datasetSensorModel supposed to be `UNIMANsnoise`
    #  stop("Error in SensorArray::initialize: dataset", datasetSensorNoiseModel, "is not loaded.")    
    #eval(parse(text = paste("mdat <-", datasetSensorNoiseModel)))

    mdat <- loadUNIMANdata(datasetSensorNoiseModel)
    
    Bsd <- mdat$Bsd
  
    if(enableSorption) Bsd <- Bsd[["Sensor"]]
    else Bsd <- Bsd[["SensorModel"]]
  
    if(!(model %in% names(Bsd)))
      stop("Error in SensorArray::initialize: 'datasetSensorNoiseModel' doesn't have entry for model '", model, "'.", sep='')
    
    sndata <- Bsd[[model]]
  }  

  # load data (3) 'datasetDistr'    
  if(is.null(coefsd)) {
    #data(list=datasetDistr, package=pck, envir=environment()) # -> 'UNIMANdistr'
    #if(!(exists("UNIMANdistr")))
    #  stop("Error in SensorArray::initialize: 'datasetDistr' is not loaded; variable 'UNIMANdistr' is not found.")    

    #if(!exists(datasetDistr)) # datasetDistr supposed to be `UNIMANdistr`
    #  stop("Error in SensorArray::initialize: dataset", datasetDistr, "is not loaded.")    
    #eval(parse(text = paste("mdat <-", datasetDistr)))
    
    mdat <- loadUNIMANdata(datasetDistr)
      
    if(enableSorption) mdat <- mdat[["uniform"]][["Sensor"]]
    else mdat <- mdat[["uniform"]][["SensorModel"]]
  
    if(!(model %in% names(mdat)))
      stop("Error in SensorArray::initialize: 'datasetDistr' doesn't have entry for model '", model, "'.", sep='')
    coefsd <- mdat[[model]]
  }  
      
  # sub-classes 
  sub.classes <- subClasses(class(.Object)) # from 'ChemoSensorArraysClassMethods.R'
  # -1- Sorption Model
  if("SorptionModel" %in% sub.classes) {
    obj <- new("SorptionModel", gases=gases, gnames=gnames, concUnits=concUnits, concUnitsInt=concUnitsIntSorption, 
      knum=knum, ...)
    for(s in slotNames(obj)) {
      slot(.Object, s) <- slot(obj, s)          
    }    
    
    if(enableSorption) { 
      conc <- Conc0    
      conc <- conc[, gases(obj)] # filter by 'gases'
    
      Conc <- predict(obj, conc=conc, concUnits=concUnits)  
    }
  }  
  # -2- Sensor Model
  if("SensorModel" %in% sub.classes) {
    obj <- new("SensorModel", 
      model=model, # class-specific parameters for SensorNoiseModel
      num=num, gases=gases, gnames=gnames, concUnits=concUnits, concUnitsInt=concUnitsInt, 
      Conc0=Conc0, Conc=Conc, dat=dat, coefsd=coefsd, ...)
    for(s in slotNames(obj)) {
      slot(.Object, s) <- slot(obj, s)
    }    
  }
  
  # -3- Other sub-classes      
  for(cl in sub.classes) {    
    if((cl != "SorptionModel" &  cl != "SensorModel")) {
      obj <- new(cl, 
        num=num, gases=gases, gnames=gnames, concUnits=concUnits, concUnitsInt=concUnitsInt, 
        sndata=sndata, # class-specific parameters for SensorNoiseModel
        ...)
      for(s in slotNames(obj)) {
        slot(.Object, s) <- slot(obj, s)
      }
    }
  }
  
  # assign
  .Object@type <- type
  .Object@idx <- idx
  .Object@enableSorption <- enableSorption  

  validObject(.Object)      
  return(.Object)
})

#' Wrapper function SensorArray.
#'
#' @name SensorArray
#' @rdname SensorArray-class
#' @param ... parameters of constructor.
#' @export
SensorArray <- function(...) new("SensorArray", ...) 

#SensorArray <- function(num, nsensors, model, gases, gnames, concUnits, csd, ssd, dsd, ...) #
#{
#  SensorArray.new <- function(...) new("SensorArray", ...)
#  
#  args <- as.list(match.call())
#  args <- args[-1] # remove 1st element
#  
#  do.call(SensorArray.new, args)
#}

#' Wrapper function Sensor
#'
#' @name Sensor
#' @rdname SensorArray-class
#' @param num Type of sensors (or UNIMAN number).
#' @seealso \code{\link{SensorArray}}, \code{\link{SensorModel}}
#' @example inst/examples/Sensor-class.R
#' @export
Sensor <- function(num = 1, ...)
{
  if(length(num) > 1)
    stop("Error in 'Sensor': 'num' is a vector.")
  new("SensorArray", num=num, ...)
}


#----------------------------
# Convert Methods
#----------------------------

#' @rdname SensorArray-class
#' @aliases getSensor,SensorArray-method
setMethod("getSensor", "SensorArray", function(object, index) 
{
  stopifnot(index %in% object@idx)
  
  sensor <- object
  sensor@idx <- 1
  sensor@num <- sensor@num[index]
  sensor@fnum <- sensor@fnum[index]      
  sensor@dataModel <- sensor@dataModel[index]
  sensor@knum <- sensor@knum[index]
  sensor@srdata <- sensor@srdata[index, , , drop = FALSE]

  sensor@sorptionModel$K <- sensor@sorptionModel$K[, index, drop = FALSE]
  sensor@sorptionModel$Q <- sensor@sorptionModel$Q[, index, drop = FALSE]
  
  return(sensor)
})


#----------------------------
# Get/Set Methods
#----------------------------

#----------------------------
# Plot Methods
#----------------------------

#' @rdname plot-methods
#' @aliases plot,SensorArray-method
setMethod("plot", "SensorArray", function (x, y, ...) 
{
  nsensors <- nsensors(x)
  # missing
  if(missing(y)) y <- ifelse(nsensors == 1, "response", "polar")

  yval <- c("response", "timeline", "pca", "prediction", "snoise", 
    "affinity", "affinityMap", "affinitySpace",  
    "polar")
  
  # check par
  match.arg(y, yval)
  
  switch(y,
    prediction = plot.SensorArray.prediction(x, y, ...),
    snoise = plot.SensorArray.snoise(x, y, ...), 
    response = plot.SensorArray.response(x, y, ...),
    timeline = plot.SensorArray.timeline(x, y, ...),    
    pca = plotPCA(x, y, ...),      
    affinity = plot.SensorArray.affinity(x, y, ...),
    affinityMap = plot.SensorArray.affinityMap(x, y, ...),     
    affinitySpace = plot.SensorArray.affinitySpace(x, y, ...),                  
    polar = plot.SensorArray.polar(x, y, ...),     
    stop("Error in SensorArray::plot: plot type 'y' is unknown. Supported types: ", 
      paste(yval, collapse=", "), "."))
})

plot.SensorArray.prediction <- function(x, y, main = "Sensor Array: PCA scoreplot", ...)
{
  plotPCA(x, y, main=main, ...)
}

plot.SensorArray.snoise <- function(x, y, conc, 
  main = paste("SensorArray: SensorArray Noise, ssd", ssd(x)), xlab = "Sample", ylab="SensorArray Signal", ...)
{
  if(missing(conc)) conc <- concSample(x, "const")
  
  X <- predict(x, conc, ...)  

  # points
  plot(X, bty='n',
    main=main, xlab = xlab, ylab = ylab, ...)  
}

plot.SensorArray.response <- function(x, y,  
  type="inc", 
  lwd = 2, lty = 1, 
  main = "Sensor Array: response", ...)
{   
  plotResponse(x, y, type=type, lwd = lwd, lty = lty, main=main, ...)
}

plot.SensorArray.timeline <- function(x, y,  
  main = "Sensor Array: signal over time", ...)
{   
  plotTimeline(x, y, main=main, ...)
}

plot.SensorArray.affinity <- function(x, y,  
  main = "Sensor Array: Affinity", ...)
{   
  plotAffinity(x, y, main=main, ...)
}

plot.SensorArray.affinitySpace <- function(x, y,  
  main = "Sensor Array: Affinity Space", ...)
{   
  plotAffinitySpace(x, y, main=main, ...)
}

plot.SensorArray.affinityMap <- function(x, y,  
  main = "Sensor Array: Affinity Map", ...)
{   
  plotAffinityMap(x, y, main=main, ...)
}

plot.SensorArray.polar <- function(x, y,  
  main = "Sensor Array: Polar plot", ...)
{   
  plotPolar(x, y, main=main, ...)
}
#----------------------------
# Predict Methods
#----------------------------

#' @rdname get-methods
#' @aliases coefficients,SensorArray-method
setMethod("coefficients", "SensorArray", function(object, type, ...)
{
  if(missing(type)) type <- "SensorModel"
  coef <- switch(type,
    SensorModel = coefficients(as(object, "SensorModel")),
    SorptionModel = object@sorptionModel$K,
    stop("Error in SensorArray::coefficients: 'type' is unknown."))  

  return(coef)
})

#' @rdname model-methods
#' @aliases predict,SensorArray-method
setMethod("predict", "SensorArray", function(object, conc, coef="numeric", concUnits="default", ...)
{
  if(missing(coef)) coef <- coef(object)
  if(concUnits == "default") concUnits <- concUnits(object)
  
  ngases <- ngases(object)
    
  if(is.null(dim(conc))) conc <- matrix(conc, ncol=ngases) # numeric vector
  if(ncol(conc) != ngases)
    stop("Error in SensorArray::predict: dimension of 'conc' is incorrect.")  

  nsensors <- nsensors(object)
  n <- nrow(conc)

  if(concUnits != concUnitsInt(object)) conc <- concNorm(object, conc, concUnits) # concNorm
  conc <- concModel(object, conc=conc, concUnits=concUnitsInt(object), ...)  # concModel
  
  if(ssd(object)) {
    coef <- predict(as(object, "SensorNoiseModel"), coef=coef, n=n, ...) # SensorArray noise   
    if(nsensors == 1) coef <- array(coef, c(dim(coef), 1))

    sdata <- sdataModel(object, conc=conc, coef=coef, concUnits=concUnitsInt(object), ...)
  }
  else {
    sdata <- sdataModel(object, conc=conc, coef=coef, concUnits=concUnitsInt(object), ...)  # sdataModel
  }

  if(dsd(object)) {
    sdata <- predict(as(object, "DriftNoiseModel"), sdata=sdata, ...) # Drift Noise Model
  }
  
  colnames(sdata) <- snames(object)
    
  return(sdata)
})

#----------------------------
# Model parameters
#----------------------------


#----------------------------
# Model Methods
#----------------------------

### Method concModel
setMethod("concModel", "SensorArray", function(object, conc, concUnits="default", ...)
{
  if(concUnits == "default") concUnits <- concUnits(object)
  if(missing(conc)) 
    stop("Error in SensorArray::predict: 'conc' is missing.")
  
  if(concUnitsInt(object) != concUnits)
    stop("Error in SensorArray::concModel: 'concUnits' is different from slot 'concUnitsInt'.")    
  
  if(csd(object)) { 
    conc <- concModel(as(object, "ConcNoiseModel"), conc=conc, concUnits=concUnits, ...)  # Concentration Noise Model
  }
  
  if(enableSorption(object)) { 
    conc <- predict(as(object, "SorptionModel"), conc=conc, concUnits=concUnits, ...)  # Sorption Model
  }
      
  return(conc)  
})

### Method sdataModel
setMethod("sdataModel", "SensorArray", function(object, conc, coef, concUnits="default", ...)
{  
  if(missing(coef)) coef <- coef(object)
  if(concUnits == "default") concUnits <- concUnits(object)
  
  if(concUnitsInt(object) != concUnits)
    stop("Error in SensorArray::sdataModel: 'concUnits' is different from slot 'concUnitsInt'.")    

  sdataModel(as(object, "SensorModel"), conc=conc, coef=coef, concUnits=concUnits, ...)  
})

