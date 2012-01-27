# last revision: 10 Jan 2012

#' @include SensorArrayModelClass.R
NULL

#----------------------------
# Class constructor
#----------------------------

#' Get default constructor parameters of class \code{\link{SensorArrayModel}}.
#' @name defaultParSensorArrayModel
#' @rdname pub-defaultParSensorArrayModel
#' @keywords SensorArrayModel defaults
#' @return List of the default parameters.
#@example R/example/defaultParSensorArrayModel.R
#' @export
defaultParSensorArrayModel <- function()
{
  par <- list(num=1:2, gases=1:3, gnames=LETTERS[1:3], concUnits=defaultConcUnits(), concUnitsInt=defaultConcUnitsInt(),
    datasetSensorModel=defaultDataSensorModel(), pck=defaultDataPackage(), 
    model="mvr", coeffNonneg = FALSE, coeffNonnegTransform = "zero",
    Conc0=NULL, Conc=NULL, dat=NULL)
  
  return(par)
}

### Constructor of SensorArrayModel class.
setMethod("initialize", "SensorArrayModel", function(.Object,
  # common for sub-classes
  num="numeric", gases="numeric", gnames="character", concUnits="character", concUnitsInt="character",
  # specific for class SensorArrayModel
  datasetSensorModel="character", pck="character", 
  model="DataModel", coeffNonneg = "logical", coeffNonnegTransform = "character",
  Conc0, Conc, dat, ...)
{   
  # missing
  def.par <- defaultParSensorArrayModel()
  
  if(missing(num)) num <- def.par$num
  if(missing(gases)) gases <- def.par$gases
  if(missing(gnames)) gnames <- def.par$gnames
  if(missing(concUnits)) concUnits <- def.par$concUnits
  if(missing(concUnitsInt)) concUnitsInt <- def.par$concUnitsInt
  if(missing(datasetSensorModel)) datasetSensorModel <- def.par$datasetSensorModel
  if(missing(pck)) pck <- def.par$pck
  if(missing(model)) model <- def.par$model
  if(missing(coeffNonneg)) coeffNonneg <- def.par$coeffNonneg
  if(missing(coeffNonnegTransform)) coeffNonnegTransform <- def.par$coeffNonnegTransform
  if(missing(Conc0)) Conc0 <- def.par$Conc0
  if(missing(Conc)) Conc <- def.par$Conc
  if(missing(dat)) dat <- def.par$dat
  
  # check 'model'
  is.correct.model <- (model %in% SensorModelNames())
  if(!is.correct.model)
    stop("Error in SensorArrayModel::initialize: name of 'model' is incorrect.")  
   
  # load data
  if(is.null(Conc0) | is.null(dat)) {
    data(list=datasetSensorModel, package=pck, envir=environment()) # -> 'C', 'dat', 'dat.corrected'
    if(!(exists("C") & exists("dat") & exists("dat.corrected")))
      stop("Error in SensorArrayModel::initialize: 'datasetSensorModel' is not loaded; variables 'C', 'dat' and 'dat.corrected' not found.")    
    Conc0 <- C
    # dat == dat
  }
  if(is.null(Conc)) Conc <- C

  # check 'num' / set up 'idx' / 'sdat'
  # set 'nsensors'
  if(sum(num <= 0 | num > ncol(dat)))
    stop("Error in SensorArrayModel::initialize: 'num' is incorrect.")  
  idx <- 1:length(num)
  sdat <- dat[, num]

  nsensors <- length(num)

  # transfrom 'Conc' and 'Conc0' from 2D to 3D
  if(length(dim(Conc0)) == 2) Conc0 <- array(Conc0, c(dim(Conc0), nsensors))
  if(length(dim(Conc)) == 2) Conc <- array(Conc, c(dim(Conc), nsensors))

  # filter by 'gases'  
  gnames <- gnames[gases]
  ngases <- length(gases)
  gind <- rep(NA, max(gases))
  for(i in 1:length(gases)) {
    g <- gases[i]
    gind[g] <- i
  }
  
  if(dim(Conc0)[2] > ngases) {
    Conc0 <- Conc0[, gases, ]
  }
  #if(is.null(ncol(Conc0))) Conc0 <- matrix(Conc0, ncol=1)
  #colnames(Conc0) <- gnames  

  if(dim(Conc)[2] > ngases) {
    Conc <- Conc[, gases, ]
  }
  #if(is.null(ncol(Conc))) Conc <- matrix(Conc, ncol=1)  
  #colnames(Conc) <- gnames   
  
  if(length(dim(Conc0)) != 3)
    stop("Error in SensorArrayModel::initialize: 'length(dim(Conc0) != 3'.")  
  if(length(dim(Conc)) != 3)
    stop("Error in SensorArrayModel::initialize: 'length(dim(Conc) != 3'.")  
      
  # assign
  .Object@num <- num  
  .Object@idx <- idx    
  .Object@gases <- gases
  .Object@gind <- gind
  .Object@ngases <- ngases
  .Object@gnames <- gnames
  .Object@concUnits <- concUnits
  .Object@concUnitsInt <- concUnitsInt

  # normalize 'Conc*'
  Conc <- concNorm(.Object, Conc)
  Conc0 <- concNorm(.Object, Conc0)
  
  #.Object@conc0$min <- initConc(.Object, Conc0, "min")
  #.Object@conc0$max <- initConc(.Object, Conc0, "max")
  #.Object@conc0$crit <- initConc(.Object, Conc0, "crit")
  #.Object@conc0$sat <- initConc(.Object, Conc0, "sat", sat.factor=1.2)

  #.Object@conc$min <- initConc(.Object, Conc, "min")
  #.Object@conc$max <- initConc(.Object, Conc, "max")
  #.Object@conc$crit <- initConc(.Object, Conc, "crit")
  #.Object@conc$sat <- initConc(.Object, Conc, "sat", sat.factor=1.2)
  
  # assign 'model'  
  .Object@coeffNonneg <- coeffNonneg
  .Object@coeffNonnegTransform <- coeffNonnegTransform
    
  dataModel <- list()
  for(i in .Object@idx) {
    dataModel[[i]] <- SensorDataModel(model, sdat[, i], Conc[, , i])
  }
  .Object@dataModel <- dataModel
  
  validObject(.Object)
  return(.Object)
})

#' @export
SensorArrayModel <- function(...)
{
  new("SensorArrayModel", ...)
}

### Function 'initConc'

#' Derive the concentration range.
#'
#' Concentration values of critical, saturation, minimal and maximum levels.
#'
#' @param object A \code{\link{SensorArrayModel}} object
#' @param conc Concentration matrix.
#' @param type Type of concentration values: \code{crit}, \code{sat}, \code{min} and \code{max}. 
#' @param sat.factor Factor by wich the maximum concentration value in \code{conc} is multiplicated.
#' @return Concentration values for all gases.
#' @name initConc
#' @rdname int-initConc
#' @keywords SensorArrayModel
initConc <- function(object, conc, type, sat.factor)
{ 
  ngases <- object@ngases
  if(!is.numeric(ngases)) 
    stop("Error in 'initConc': 'ngases' is not numeric.")
  
  out.conc <- switch(type,
    crit = sapply(1:ncol(conc), function(i) { min(conc[conc[,i] > 0,i]) }),
    sat = sapply(1:ncol(conc), function(i) { sat.factor*max(conc[conc[,i] > 0,i]) }),
    min = sapply(1:ncol(conc), function(i) { min(conc[conc[,i] > 0,i]) }),
      # (optional) correction for gas B: out.conc[2] <- 0.5*out.conc[2]
    max =  sapply(1:ncol(conc), function(i) { max(conc[conc[,i] > 0,i]) }),
    stop("Error in 'initConc': 'type' is unknown."))
    
  return(out.conc)
}

initModelPlsr <- function(object, X, C, ...)
{
  model <- initDataModelPlsr(X, C, ...)
  
  return(model)
}

#----------------------------
# Plot Methods
#----------------------------
setMethod("plot", "SensorArrayModel", function (x, y, ...) 
{
  yval <- c("response")

  # missing
  if(missing(y)) y <- "response"
  
  switch(y,
    response = plot.SensorArrayModel.response(x, y, ...),
    stop("Error in SensorArrayModel::plot: plot type 'y' is unknown."))
})

plot.SensorArrayModel.response <- function(x, y,  
  lwd = 2, lty = 1,
  main = "Sensor Array Model: response", ...)
{ 
  nsensors <- nsensors(x)
  # main
  if(missing(main)) {
    if(nsensors <= 5) main <- paste(main, "\n num ", paste(num(x), collapse=", "), sep='')
    else main <- paste(main, "\n ", nsensors, " sensors", sep='')
    main <- paste(main, ", model '", modelName(x), "'", sep='')
  }
  
  plotResponse(x, y, lwd = lwd, lty = lty, main=main, ...)
}

#----------------------------
# Predict Methods
#----------------------------

### Method coefficients
setMethod("coefficients", "SensorArrayModel", function(object, ...)
{
  t(laply(object@dataModel, coef))
})

### Method ncoef
setMethod("ncoef", "SensorArrayModel", function(x)
{
  length(coef(x@dataModel[[1]]))
})

### Method predict
setMethod("predict", "SensorArrayModel", function(object, conc, coef="numeric", concUnits="default", ...)
{
  if(missing(coef)) coef <- coef(object)
  if(concUnits == "default") concUnits <- concUnits(object)
  
  if(is.null(ncol(conc))) conc <- matrix(conc, ncol=1) # 1-column case
  if(ncol(conc) != ngases(object))
    stop("Error in SensorArrayModel::predict: dimension of 'conc' is incorrect.")  
  
  if(concUnits != concUnitsInt(object)) conc <- concNorm(object, conc, concUnits) # concNorm
  sdataModel(object, conc=conc, coef=coef, concUnits=concUnitsInt(object), ...)  # sdataModel
})

#----------------------------
# Model Methods
#----------------------------

### Method sdataModel
setMethod("sdataModel", "SensorArrayModel", function(object, conc, coef="numeric", concUnits="default", ...)
{  
  if(missing(coef)) coef <- coef(object)
  if(concUnits == "default") concUnits <- concUnits(object)
    
  if(concUnitsInt(object) != concUnits)
    stop("Error in SensorArrayModel::sdataModel: 'concUnits' is different from slot 'concUnitsInt'.")    
  
  nsensors <- nsensors(object)
  ncoef <- ncoef(object)
  n <- nrow(conc)

  if(length(dim(conc)) == 2)
    conc <- array(conc, c(dim(conc), nsensors))
  
  if(length(dim(coef)) == 2) {
    if(ncol(coef) != nsensors)
      stop("Error in SensorArrayModel::sdataModel: 'ncol(coef) != nsensors'.")    
    if(nrow(coef) != ncoef)
      stop("Error in SensorArrayModel::sdataModel: 'nrow(coef) != ncoef'.")    
    
    sdata <- matrix(NA, nrow=n, ncol=nsensors)
    for(i in idx(object)) {
      sdata[, i] <- predict(object@dataModel[[i]], C=conc[, , i], B=coef[, i], ...)  
    } 
  }
  else if (length(dim(coef)) == 3){
    if(dim(coef)[3] != nsensors)
      stop("Error in SensorArrayModel::sdataModel: dim(coef)[3] != nsensors'.")    
    if(dim(coef)[2] != ncoef)
      stop("Error in SensorArrayModel::sdataModel: 'dim(coef)[2] != ncoef'.")    
    
    sdata <- matrix(NA, nrow=n, ncol=nsensors)
    for(s in idx(object)) {
      sdata[, s] <- predict(object@dataModel[[s]], C=conc[, , s], B=coef[, ,s], ...)  
    }   
  }
  else
    stop("Error in SensorArrayModel::sdataModel: 'coef' dimenstion is unknown.")    
  
  return(sdata)
})
