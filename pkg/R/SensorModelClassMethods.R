# last revision: 10 Jan 2012

#' @include SensorModelClass.R
NULL

#----------------------------
# Class constructor
#----------------------------

#' Get model names of class \code{\link{Sensor}}.
#' @name SensorModelNames
#' @rdname pub-SensorModelNames
#' @keywords SensorModel
#' @return Character vector of model names.
#@example R/example/getSensorModelNames.R
#' @export
SensorModelNames <- function()
{
  #return(c("plsr", "plsr-stick","spliner", "spliner-mid"))
  return(c("plsr", "mvr"))
}

#' Get default constructor parameters of class \code{\link{SensorModel}}.
#' @name defaultParSensorModel
#' @rdname pub-defaultParSensorModel
#' @keywords SensorModel defaults
#' @return List of the default parameters.
#@example R/example/defaultParSensorModel.R
#' @export
defaultParSensorModel <- function()
{
  par <- list(num=1, gases=1:3, gnames=LETTERS[1:3], concUnits="perc", concUnitsInt=defaultConcUnitsInt(),
    datasetSensorModel=defaultDataSensorModel(), pck=defaultDataPackage(), 
    model=defaultDataModel(), coeffNonneg = FALSE, coeffNonnegTransform = "zero",
    Conc0=NULL, Conc=NULL, dat=NULL)
  
  return(par)
}

### Constructor of SensorModel class.
setMethod("initialize", "SensorModel", function(.Object,
  # common for sub-classes
  num="numeric", gases="numeric", gnames="character", concUnits="character", concUnitsInt="character",
  # specific for class SensorModel
  datasetSensorModel="character", pck="character", 
  model="character", coeffNonneg = "logical", coeffNonnegTransform = "character",
  Conc0, Conc, dat, ...)
{   
  # missing
  def.par <- defaultParSensorModel()
  
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
    stop("Error in SensorModel::initialize: name of 'model' is incorrect.")  
     
  # load data
  if(is.null(Conc0) | is.null(dat)) {
    data(list=datasetSensorModel, package=pck, envir=environment()) # -> 'C', 'dat', 'dat.corrected'
    if(!(exists("C") & exists("dat") & exists("dat.corrected")))
      stop("Error in SensorModel::initialize: 'datasetSensorModel' is not loaded; variables 'C', 'dat' and 'dat.corrected' not found.")    
    Conc0 <- C
    # dat == dat
  }
  if(is.null(Conc)) Conc <- C
  
  # filter by 'gases'  
  gnames <- gnames[gases]
  ngases <- length(gases)
  gind <- rep(NA, max(gases))
  for(i in 1:length(gases)) {
    g <- gases[i]
    gind[g] <- i
  }
   
  if(ncol(Conc0) > ngases) {
    Conc0 <- Conc0[, gases]
  }
  if(is.null(ncol(Conc0))) Conc0 <- matrix(Conc0, ncol=1)
  colnames(Conc0) <- gnames  

  if(ncol(Conc) > ngases) {
    Conc <- Conc[, gases]
  }
  if(is.null(ncol(Conc))) Conc <- matrix(Conc, ncol=1)  
  colnames(Conc) <- gnames
  
  # filter by 'cind'  
  cind <- which(apply(Conc, 1, sum) > 0)
  Conc0 <- Conc0[cind, ]
  Conc <- Conc[cind, ]
  dat <- dat[cind, ]
    
  # check 'num' / create 'sdat'
  if(num <= 0 | num > ncol(dat))
    stop("Error in SensorModel::initialize: 'num' is incorrect.")  
  sdat <- dat[, num] 
    
  # assign
  .Object@num <- num  
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
  
  .Object@dataModel <- SensorDataModel(model, sdat, Conc)
  
  validObject(.Object)
  return(.Object)
})

#' @export
SensorModel <- function(...)
{
  new("SensorModel", ...)
}

### Function 'initConc'

#' Derive the concentration range.
#'
#' Concentration values of critical, saturation, minimal and maximum levels.
#'
#' @param object A \code{\link{SensorModel}} object
#' @param conc Concentration matrix.
#' @param type Type of concentration values: \code{crit}, \code{sat}, \code{min} and \code{max}. 
#' @param sat.factor Factor by wich the maximum concentration value in \code{conc} is multiplicated.
#' @return Concentration values for all gases.
#' @name initConc
#' @rdname int-initConc
#' @keywords SensorModel
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

initModelPlsr <- function(X, C, ...)
{
  model <- initDataModelPlsr(X, C, ...)
  
  return(model)
}

#----------------------------
# Plot Methods
#----------------------------
setMethod("plot", "SensorModel", function (x, y, ...) 
{
  yval <- c("response", "predict")

  # missing
  if(missing(y)) y <- "response"
  
  switch(y,
    predict = plot.SensorModel.predict(x, y, ...),
    response = plot.SensorModel.response(x, y, ...),
    stop("Error in SensorModel::plot: plot type 'y' is unknown."))
})

plot.SensorModel.predict <- function(x, y, conc, gases = 1, jitter=FALSE,  
  col, pch = 20, cex = 1,
  main = "Prediction: y ~ c", xlab = paste("Concentration of gas", gnames(x)[gases]), ylab="Sensor Model Response, [a.u.]", ...)
{
  if(missing(conc))
    stop("Error in 'plot.SensorModel.prediction': 'conc' is missing.")
  if(missing(col)) col <- "black" # gcol(x, gases=gases)
  
  C <- conc
  X <- predict(x, conc, ...)  

  # points
  ind <- (C[, gases] != 0)  
  cp <- C[ind, gases]
  if(jitter) cp <- jitter(cp)  
  xp <- X[ind, 1]
  if(jitter) xp <- jitter(xp)
  
  if(jitter) main <- paste(main, " (jittered points)", sep='')
  xlab <- paste(xlab, ", [", ConcUnitsStr(concUnits(x)), "]", sep='')
  
  plot(cp, xp, bty = 'n', pch = pch, cex = cex, col=col,
    main=main, xlab = xlab, ylab = ylab)  
}

plot.SensorModel.response <- function(x, y,  
  lwd = 2, lty = 1,
  main = paste("Sensor Model: response \n num ", num(x), ", model '", modelName(x), "'", sep=""), ...)
{ 
  plotResponse(x, y, lwd = lwd, lty = lty, main=main, ...)
}

#----------------------------
# Predict Methods
#----------------------------

### Method coefficients
setMethod("coefficients", "SensorModel", function(object, ...)
{
  coefficients(object@dataModel)  
})

### Method predict
setMethod("predict", "SensorModel", function(object, conc, coef="numeric", concUnits="default", ...)
{
  if(missing(coef)) coef <- coef(object)
  if(concUnits == "default") concUnits <- concUnits(object)
  
  if(is.null(ncol(conc))) conc <- matrix(conc, ncol=1) # 1-column case
  if(ncol(conc) != ngases(object))
    stop("Error in SensorModel::predict: dimension of 'conc' is incorrect.")  
  
  if(concUnits != concUnitsInt(object)) conc <- concNorm(object, conc, concUnits) # concNorm
  sdataModel(object, conc=conc, coef=coef, concUnits=concUnitsInt(object), ...)  # sdataModel
})

#----------------------------
# Model Methods
#----------------------------

### Method sdataModel
setMethod("sdataModel", "SensorModel", function(object, conc, coef="numeric", concUnits="default", ...)
{  
  if(missing(coef)) coef <- coef(object)
  if(concUnits == "default") concUnits <- concUnits(object)
    
  if(concUnitsInt(object) != concUnits)
    stop("Error in SensorModel::sdataModel: 'concUnits' is different from slot 'concUnitsInt'.")    
  
  predict(object@dataModel, C=conc, B=coef, ...)  
})
