# last revision: 10 Jan 2012

#' @include SensorClass.R
NULL

#----------------------------
# Class constructor
#----------------------------

#' Get default constructor parameters of class \code{\link{Sensor}}.
#' @name defaultParSensor
#' @rdname pub-defaultParSensor
#' @keywords Sensor defaults
#' @return List of the default parameters.
#@example R/example/defaultParSensor.R
#' @export
defaultParSensor <- function()
{
  par <- list(type = "polymeric",
    enableSorption = TRUE, 
    num = 1, gases=1:3, gnames=LETTERS[1:3], 
    concUnits="perc", concUnitsInt=defaultConcUnitsInt(), concUnitsSorption = defaultConcUnitsSorption(),
    # Sensor Model
    datasetSensorModel=defaultDataSensorModel(), datasetSensorNoiseModel=defaultDataSensorNoiseModel(), pck=defaultDataPackage(), 
    Conc0=NULL, Conc=NULL, dat=NULL, sndata=NULL)
  
  return(par)
}

setMethod("initialize", "Sensor", function(.Object,
  # specific for class Sensor
  type = "character",
  enableSorption = "logical",
  # common for sub-classes
  num="numeric", gases="numeric", gnames="character", 
  concUnits="character", concUnitsInt="character", concUnitsIntSorption="character", 
  # specific for Sorption Model
  knum="numeric",
  # specific for Sensor Model
  datasetSensorModel = "character", datasetSensorNoiseModel = "character", pck = "character", 
  Conc0, Conc, dat, sndata, ...)  
{ 
  # missing
  def.par <- defaultParSensor()

  if(missing(type)) type <- def.par$type
  if(missing(enableSorption)) enableSorption <- def.par$enableSorption
  
  # missing 'num' and 'knum'
  if(missing(num)) num <- def.par$num
  if(missing(knum)) knum <- num
  
  if(missing(gases)) gases <- def.par$gases
  if(missing(gnames)) gnames <- def.par$gnames
  if(missing(concUnits)) concUnits <- def.par$concUnits
  if(missing(concUnitsInt)) concUnitsInt <- def.par$concUnitsInt
  if(missing(concUnitsIntSorption)) concUnitsIntSorption <- def.par$concUnitsIntSorption
  
  if(missing(datasetSensorModel)) datasetSensorModel <- def.par$datasetSensorModel
  if(missing(datasetSensorNoiseModel)) datasetSensorNoiseModel <- def.par$datasetSensorNoiseModel  
  if(missing(pck)) pck <- def.par$pck
  if(missing(Conc0)) Conc0 <- def.par$Conc0
  if(missing(Conc)) Conc <- def.par$Conc
  if(missing(dat)) dat <- def.par$dat
  if(missing(sndata)) sndata <- def.par$sndata
  
  # 'idx'
  idx <- 1:length(num)
        
  # check
  if(length(num) != length(knum)) 
    stop("Error in 'Sensor::initialize': 'length(num) != length(knum)'.")    
  
  # load data (1) 'datasetSensorModel'
  if(is.null(Conc0) | is.null(dat)) {
    data(list=datasetSensorModel, package=pck, envir=environment()) # -> 'C', 'dat', 'dat.corrected'
    if(!(exists("C") & exists("dat") & exists("dat.corrected")))
      stop("Error in Sensor::initialize: 'datasetSensorModel' is not loaded; variables 'C', 'dat' and 'dat.corrected' not found.")    
    Conc0 <- C
    # dat == dat
  }
  if(is.null(Conc)) Conc <- C
  if(sum(dim(Conc0) == dim(Conc)) != 2)
    stop("Error in Sensor::initialize: dimension of 'Conc0' and 'Conc' are different.")    

  # check 'num'
  if(num <= 0 | num > ncol(dat))
    stop("Error in Sensor::initialize: 'num' is incorrect.")  

  # load data (2) 'datasetSensorModel'    
  if(is.null(sndata)) {
    data(list=datasetSensorNoiseModel, package=pck, envir=environment()) # -> 'Bsd'
    if(!(exists("Bsd")))
      stop("Error in Sensor::initialize: 'datasetSensorNoiseModel' is not loaded; variable 'Bsd' is not found.")    

    sndata <- switch(as.numeric(enableSorption),
      '1' = Bsd[["Sensor"]][["plsr"]],
      '0' = Bsd[["SensorModel"]][["plsr"]])
  }  
        
  # sub-classes 
  sub.classes <- subClasses(class(.Object)) # from 'ChemosensorsClassMethods.R'
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
    obj <- new("SensorModel", num=num, gases=gases, gnames=gnames, concUnits=concUnits, concUnitsInt=concUnitsInt, 
      Conc0=Conc0, Conc=Conc, dat=dat, ...)
    for(s in slotNames(obj)) {
      slot(.Object, s) <- slot(obj, s)
    }    
  }
  # -3- Prepare data
  
  # -4- Other sub-classes      
  for(cl in sub.classes) {    
    if((cl != "SorptionModel" & cl != "SensorModel")) {
      obj <- new(cl, num=num, gases=gases, gnames=gnames, concUnits=concUnits, concUnitsInt=concUnitsInt, 
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

#' @export
Sensor <- function(...)
{
  new("Sensor", ...)
}

#----------------------------
# Get/Set Methods
#----------------------------

#----------------------------
# Plot Methods
#----------------------------
setMethod("plot", "Sensor", function (x, y, ...) 
{
  yval <- c("response", "prediction", "snoise")
  # missing
  if(missing(y)) y <- "response"
  
  switch(y,
    prediction = plot.Sensor.prediction(x, y, ...),
    snoise = plot.Sensor.snoise(x, y, ...), 
    response = plot.Sensor.response(x, y, ...), 
    stop("Error in Sensor::plot: plot type 'y' is unknown. Supported types: ", 
      paste(yval, collapse=", "), "."))
})

plot.Sensor.prediction <- function(x, y, conc, gas = 1, jitter=FALSE, 
  main = "Prediction: y ~ c", xlab = paste("Concentration of gas", gnames(x)[gas]), ylab="Sensor Signal", ...)
{
  if(missing(conc))
    stop("Error in 'plot.Sensor.prediction': 'conc' is missing.")
  
  X <- predict(x, conc, ...)  

  # points
  ind <- (conc[, gas] != 0)  
  cp <- conc[ind, gas]
  if(jitter) cp <- jitter(cp)  
  xp <- X[ind, 1]
  if(jitter) xp <- jitter(xp)
  
  if(jitter) main <- paste(main, " (jittered points)", sep='')
  plot(cp, xp, bty='n',
    main=main, xlab = xlab, ylab = ylab, ...)  
}

plot.Sensor.snoise <- function(x, y, conc, 
  main = paste("Sensor: Sensor Noise, ssd", ssd(x)), xlab = "Sample", ylab="Sensor Signal", ...)
{
  if(missing(conc)) conc <- concSample(x, "const")
  
  X <- predict(x, conc, ...)  

  # points
  plot(X, bty='n',
    main=main, xlab = xlab, ylab = ylab, ...)  
}

plot.Sensor.response <- function(x, y,  
  lwd = 2, lty = 1, type="inc",
  main = "Sensor: response", ...)
{   
  plotResponse(x, y, type = type, lwd = lwd, lty = lty, main=main, ...)
}
#----------------------------
# Predict Methods
#----------------------------

### Method coefficients
setMethod("coefficients", "Sensor", function(object, type, ...)
{
  if(missing(type)) type <- "SensorModel"
  coef <- switch(type,
    SensorModel = coefficients(as(object, "SensorModel")),
    stop("Error in Sensor::coefficients: 'type' is unknown."))  

  return(coef)
})

### Method predict
setMethod("predict", "Sensor", function(object, conc, coef="numeric", concUnits="default", ...)
{
  if(missing(coef)) coef <- coef(object)
  if(concUnits == "default") concUnits <- concUnits(object)
  
  if(is.null(ncol(conc))) conc <- matrix(conc, ncol=1) # 1-column case
  if(ncol(conc) != ngases(object))
    stop("Error in Sensor::predict: dimension of 'conc' is incorrect.")  

  n <- nrow(conc)
  ngases <- ngases(object)
  
  if(concUnits != concUnitsInt(object)) conc <- concNorm(object, conc, concUnits) # concNorm
  conc <- concModel(object, conc=conc, concUnits=concUnitsInt(object), ...)  # concModel
  if(ssd(object)) {
    coef <- predict(as(object, "SensorNoiseModel"), coef=coef, n=n, ...) # sensor noise
    #sdata <- matrix(NA, nrow=n, ncol=1)
    #for(i in 1:nrow(coef)) {    
    #  sdata[i, ] <- as.numeric(sdataModel(object, conc=matrix(conc[i, ], ncol=ngases), coef=coef[i, ],
    #    concUnits=concUnitsInt(object)))
    #}
    sdata <- sdataModel(object, conc=conc, coef=coef, concUnits=concUnitsInt(object), ...)
    
  }
  else {
    sdata <- sdataModel(object, conc=conc, coef=coef, concUnits=concUnitsInt(object), ...)  # sdataModel
  }
  
  return(sdata)
})

#----------------------------
# Model parameters
#----------------------------


#----------------------------
# Model Methods
#----------------------------

### Method concModel
setMethod("concModel", "Sensor", function(object, conc, concUnits="default", ...)
{
  if(concUnits == "default") concUnits <- concUnits(object)
  if(missing(conc)) 
    stop("Error in Sensor::predict: 'conc' is missing.")
  
  if(concUnitsInt(object) != concUnits)
    stop("Error in Sensor::concModel: 'concUnits' is different from slot 'concUnitsInt'.")    
  
  if(csd(object)) { 
    conc <- concModel(as(object, "ConcNoiseModel"), conc=conc, concUnits=concUnits, ...)  # Concentration Noise Model
  }
  
  if(enableSorption(object)) { 
    conc <- predict(as(object, "SorptionModel"), conc=conc, concUnits=concUnits, ...)  # Sorption Model
  }
      
  return(conc)  
})

### Method sdataModel
setMethod("sdataModel", "Sensor", function(object, conc, coef, concUnits="default", ...)
{  
  if(missing(coef)) coef <- coef(object)
  if(concUnits == "default") concUnits <- concUnits(object)
  
  if(concUnitsInt(object) != concUnits)
    stop("Error in Sensor::sdataModel: 'concUnits' is different from slot 'concUnitsInt'.")    

  sdataModel(as(object, "SensorModel"), conc=conc, coef=coef, concUnits=concUnits, ...)  
})

