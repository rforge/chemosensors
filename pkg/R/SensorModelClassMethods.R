# last revision: 10 Jan 2012

#' @include SensorModelClass.R
NULL

#----------------------------
# Class constructor
#----------------------------

#' Function to get model names of class \code{\link{SensorModel}}.
#' @rdname SensorModel-class
#' @aliases SensorModelNames
#' @return Character vector of model names.
#' @export
SensorModelNames <- function()
{
  #return(c("plsr", "plsr-stick","spliner", "spliner-mid"))
  return(c("plsr", "mvr", "broken-stick", "ispline"))
}

#' Function to get default constructor parameters of class \code{\link{SensorModel}}.
#' @rdname SensorModel-class
#' @aliases defaultParSensorModel
#' @return List of the default parameters.
#' @export
defaultParSensorModel <- function()
{
  par <- list(num=1, gases=1:3, gnames=LETTERS[1:3], concUnits="perc", concUnitsInt=defaultConcUnitsInt(),
    datasetSensorModel=defaultDataSensorModel(), datasetDistr=defaultDataDistr(), pck=defaultDataPackage(), 
    model=defaultDataModel(), coeffNonneg = FALSE, coeffNonnegTransform = "zero",
    coefsd = NULL,
    Conc0=NULL, Conc=NULL, dat=NULL,
    tunit = 1,
    beta = 2)
  
  return(par)
}

#' Constructor method of SensorModel Class.
#'
#' @name SensorModel
#' @rdname SensorModel-class
setMethod("initialize", "SensorModel", function(.Object,
  # common for sub-classes
  nsensors="numeric", num="numeric", gases="numeric", gnames="character", concUnits="character", concUnitsInt="character",
  # specific for class SensorModel
  datasetSensorModel="character", datasetDistr = "character", pck="character", 
  coefsd,
  model="character", coeffNonneg = "logical", coeffNonnegTransform = "character",
  Conc0, Conc, dat, 
  tunit = "numeric", beta = "numeric", ...)
{   
  # missing
  def.par <- defaultParSensorModel()
  
  # missing 'nsensors', 'num'
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

  # set 'model' and 'coeffNonneg'/'coeffNonnegTransform'
  if(missing(model)) model <- def.par$model 
  # 'coeffNonneg'
  missing.coeffNonneg <- missing(coeffNonneg)
  if(missing.coeffNonneg) coeffNonneg.par <- def.par$coeffNonneg
  else coeffNonneg.par <- coeffNonneg
  coeffNonneg <- initCoeffNonneg(model, missing.coeffNonneg, coeffNonneg.par)
  # 'coeffNonnegTransform'  
  if(missing(coeffNonnegTransform)) coeffNonnegTransform <- def.par$coeffNonnegTransform  
    
  if(missing(gases)) gases <- def.par$gases
  if(missing(gnames)) gnames <- def.par$gnames
  if(missing(concUnits)) concUnits <- def.par$concUnits
  if(missing(concUnitsInt)) concUnitsInt <- def.par$concUnitsInt
  if(missing(datasetSensorModel)) datasetSensorModel <- def.par$datasetSensorModel
  if(missing(datasetDistr)) datasetDistr <- def.par$datasetDistr  
  if(missing(pck)) pck <- def.par$pck
  if(missing(coefsd)) coefsd <- def.par$coefsd  

  if(missing(Conc0)) Conc0 <- def.par$Conc0
  if(missing(Conc)) Conc <- def.par$Conc
  if(missing(dat)) dat <- def.par$dat

  if(missing(tunit)) tunit <- def.par$tunit
  
  if(missing(beta)) beta <- def.par$beta    
  
  # check 'model'
  is.correct.model <- (model %in% SensorModelNames())
  if(!is.correct.model)
    stop("Error in SensorModel::initialize: name of 'model' is incorrect.")  
     
   # load data (1) 'datasetSensorModel'
  if(is.null(Conc0) | is.null(dat)) {
    #data(list=datasetSensorModel, package=pck, envir=environment()) # -> 'C', 'dat', 'dat.corrected'
    #if(!(exists("C") & exists("dat") & exists("dat.corrected")))
    #  stop("Error in SensorModel::initialize: 'datasetSensorModel' is not loaded; variables 'C', 'dat' and 'dat.corrected' not found.")    
    
    #print(ls(pos = 3))
    mdat <- loadUNIMANdata(datasetSensorModel)

    #if(!exists(datasetSensorModel)) # datasetSensorModel supposed to be `UNIMANshort` 
    #  stop("Error in SensorModel::initialize: dataset ", datasetSensorModel, " is not loaded.")    
    #eval(parse(text = paste("mdat <-", datasetSensorModel)))

    #if(!exists(datasetSensorModel)) {
    #  mdat <- NULL
    #} else {
    #  eval(parse(text = paste("mdat <-", datasetSensorModel)))
    #}

    C <- mdat$C
    dat <- mdat$dat
    dat.corrected <- mdat$dat.corrected
    
    Conc0 <- C
    # dat == dat
  }
  
  if(is.null(Conc)) Conc <- C
  
  
  # check 'num' / set up 'idx' / 'sdat'
  # set 'nsensors'
  if(sum(num <= 0 | num > ncol(dat)))
    stop("Error in SensorModel::initialize: 'num' is incorrect.")  
  idx <- 1:length(num)
  sdat <- dat[, num]
  if(is.null(dim(sdat))) sdat <- matrix(sdat, ncol=1) # 1-column case
  
  nsensors <- length(num)
  fnum <- unique(num)

  # filter by 'gases'  
  gnames <- gnames[gases]
  ngases <- length(gases)
  gind <- rep(NA, max(gases))
  for(i in 1:length(gases)) {
    g <- gases[i]
    gind[g] <- i
  }
  
  # transfrom 'Conc' and 'Conc0' from 2D to 3D
  
  if(ncol(Conc0) > ngases) Conc0[, gases]
  Conc0 <- array(Conc0, c(NROW(Conc0), ngases, nsensors))
  if(ncol(Conc) > ngases) Conc <- Conc[, gases]
  Conc <- array(Conc, c(NROW(Conc), ngases, nsensors))  
  
  if(length(dim(Conc0)) != 3)
    stop("Error in SensorModel::initialize: 'length(dim(Conc0) != 3'.")  
  if(length(dim(Conc)) != 3)
    stop("Error in SensorModel::initialize: 'length(dim(Conc) != 3'.")  
  
  # init. sub-classes      
  sub.classes <- subClasses(class(.Object)) # from 'ChemoSensorArraysClassMethods.R'
  for(cl in sub.classes) {    
    obj <- new(cl, 
      num=num, gases=gases, gnames=gnames, concUnits=concUnits, concUnitsInt=concUnitsInt, 
      tunit=tunit, ...)
    for(s in slotNames(obj)) {
      slot(.Object, s) <- slot(obj, s)
    }
  }
    
  # assign
  .Object@beta <- beta  
  .Object@num <- num
  .Object@fnum <- fnum      
  .Object@idx <- idx    
  .Object@gases <- gases
  .Object@gind <- gind
  .Object@ngases <- ngases
  .Object@gnames <- gnames
  .Object@concUnits <- concUnits
  .Object@concUnitsInt <- concUnitsInt

  .Object@coeffNonneg <- coeffNonneg
  .Object@coeffNonnegTransform <- coeffNonnegTransform
    
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
  
  # assign 'coefsd' & load data (2)  
  if(is.null(coefsd)) {
    #data(list=datasetDistr, package=pck, envir=environment()) # -> 'UNIMANdistr'
    #if(!exists("UNIMANdistr"))
    #  stop("Error in SensorModel::initialize: 'datasetDistr' is not loaded; variable 'UNIMANdistr' not found.")    
   
    #if(!exists(datasetDistr)) # datasetDistr supposed to be `UNIMANdistr`
    #  stop("Error in SensorModel::initialize: dataset", datasetDistr, "is not loaded.")    

    #eval(parse(text = paste("mdat <-", datasetDistr)))
    
    mdat <- loadUNIMANdata(datasetDistr)
        
    mdat <- mdat[['uniform']][['SensorModel']]
    if(!(model %in% names(mdat)))
      stop("Error in SensorModel::initialize: data for 'model' are missed in 'UNIMANdistr'.")    
    coefsd <- mdat[[model]]
    
    ngases0 <- 3
    ncoef0 <- as.integer(length(coefsd) / ngases0)
    ind <- as.numeric(sapply(gases, function(i, ncoef0) seq(i, by=ncoef0, length=ncoef0), ncoef0))
    ind <- sort(ind)
    coefsd <- coefsd[ind]
  }
  
  coefsd <- matrix(coefsd, nrow=length(coefsd), ncol=17)
    
  .Object@coefsd <- coefsd   
 
  # assign 'model'  
  dataModel <- list()
  for(i in .Object@idx) {
    dataModel[[i]] <- SensorDataModel(model, sdat[, i], Conc[, , i], 
      coeffNonneg=coeffNonneg, coeffNonnegTransform=coeffNonnegTransform)

    # simulate coef.
    if(length(.Object@fnum) < length(.Object@num)) {
      coefi <- coef(dataModel[[i]])
      ncoef <- length(coefi)
      coefi.sd <- .Object@beta * .Object@coefsd[, .Object@num[i]] # beta
      
      coefi.min <- coefi - coefi.sd
      coefi.min[coefi.min < 0] <- 0
      coefi.max <- coefi + coefi.sd
    
      coefi.sim <- runif(ncoef, min=coefi.min, max=coefi.max)
    
      dataModel[[i]]$coefficients <- coefi.sim
    }
  }
  .Object@dataModel <- dataModel
  
  validObject(.Object)
  return(.Object)
})

#' Wrapper function SensorModel.
#'
#' @name SensorModel
#' @rdname SensorModel-class
#' @param ... parameters of constructor.
#' @export
SensorModel <- function(...)
{
  new("SensorModel", ...)
}

### Function 'initConc'

# Derive the concentration range.
#
# Concentration values of critical, saturation, minimal and maximum levels.
#
# @param object A \code{\link{SensorModel}} object
# @param conc Concentration matrix.
# @param type Type of concentration values: \code{crit}, \code{sat}, \code{min} and \code{max}. 
# @param sat.factor Factor by wich the maximum concentration value in \code{conc} is multiplicated.
# @return Concentration values for all gases.
# @name initConc
# @rdname int-initConc
# @keywords SensorModel
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

initCoeffNonneg <- function(model, missing.coeffNonneg, coeffNonneg.par)
{
  coeffNonneg.int <- switch(model, 
    'plsr' = FALSE, 
    'mvr' = TRUE, 'broken-stick' = TRUE, 'ispline' = TRUE,
    stop("Error in 'SensorModel::initCoeffNonneg': 'model' is unknown."))
  
  if(missing.coeffNonneg) coeffNonneg.par <- coeffNonneg.int
  
  if(model == 'plsr' & coeffNonneg.par != coeffNonneg.int)
    stop("Error in 'SensorModel::initCoeffNonneg': for model (", model, ") coeffNonneg.par (", coeffNonneg.par, 
    ") != ", " coeffNonneg.int (", coeffNonneg.int, ").")
  
  return(coeffNonneg.par)
}

#----------------------------
# Plot Methods
#----------------------------

#' @rdname plot-methods
#' @aliases plot,SensorModel-method
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
  main = "Sensor Model: response", ...)
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

#' @rdname get-methods
#' @aliases coefficients,SensorModel-method
setMethod("coefficients", "SensorModel", function(object, ...)
{
  nsensors <- nsensors(object)
  ncoef <- ncoef(object)
  coef <- matrix(NA, nrow=ncoef, ncol=nsensors)
  for(s in 1:nsensors)
    coef[, s] <- coef(object@dataModel[[s]])
    
  return(coef)
})

#' @rdname get-methods
#' @aliases ncoef,SensorModel-method
setMethod("ncoef", "SensorModel", function(x)
{
  length(coef(x@dataModel[[1]]))
})


#' @rdname model-methods
#' @aliases predict,SensorModel-method
setMethod("predict", "SensorModel", function(object, conc, coef="numeric", concUnits="default", ...)
{
  if(missing(coef)) coef <- coef(object)
  if(concUnits == "default") concUnits <- concUnits(object)
  
  ngases <- ngases(object)

  if(is.null(dim(conc))) conc <- matrix(conc, ncol=ngases) # numeric vector    
  if(ncol(conc) != ngases)
    stop("Error in SensorModel::predict: dimension of 'conc' is incorrect.")  
  
  if(concUnits != concUnitsInt(object)) conc <- concNorm(object, conc, concUnits) # concNorm
  sdataModel(object, conc=conc, coef=coef, concUnits=concUnitsInt(object), ...)  # sdataModel
})

#----------------------------
# Model Methods
#----------------------------

### Method sdataModel
setMethod("sdataModel", "SensorModel", function(object, conc, coef="numeric", concUnits="default", enableDyn = "logical", 
  cores = getOption("cores"), nclusters, ...)
{  
  if(missing(coef)) coef <- coef(object)
  if(concUnits == "default") concUnits <- concUnits(object)
  if(missing(enableDyn)) enableDyn <- enableDyn(object)
      
  if(concUnitsInt(object) != concUnits)
    stop("Error in SensorModel::sdataModel: 'concUnits' is different from slot 'concUnitsInt'.")    

  if(is.null(cores)) {
    if(!missing(nclusters)) { 
      cores <- nclusters 
    } else {
      cores <- 1
    }
  }
  
  if(cores > 1) {
    cat(" * Started computing in parallel on", cores, "CPU cores (if available) (SensorModel::sdataModel).\n")
    if(!require(doMC)) {
        stop("Package `doMC` is required for parallel computing.")
    }
    else {
      doMC::registerDoMC(cores) 
    }
  }

  ngases <- ngases(object)
  
  if(is.null(dim(conc))) conc <- matrix(conc, ncol=ngases) # numeric vector    
  
  nsensors <- nsensors(object)
  ncoef <- ncoef(object)
  n <- nrow(conc)

  #enableDyn <- enableDyn(object)
  out <- ifelse(enableDyn, 'gas', 'sum')

  if(length(dim(conc)) == 2)
    conc <- array(conc, c(dim(conc), nsensors))
  
  # case 1: ssd == 0
  if(length(dim(coef)) == 2) {
    if(ncol(coef) != nsensors)
      stop("Error in SensorModel::sdataModel: 'ncol(coef) != nsensors'.")    
    if(nrow(coef) != ncoef)
      stop("Error in SensorModel::sdataModel: 'nrow(coef) != ncoef'.")    
    
    sdata <- matrix(NA, nrow=n, ncol=nsensors)
    
    run.sdata <- function(i)
    {
      conci <- conc[, , i]
      sdatai <- predict(object@dataModel[[i]], C=conci, B=coef[, i], out = out) 
      if(enableDyn) {
        # v1
        sdata.pulse <- sdata2pulse(object, conci, sdatai)
        sdata.out <- predict(as(object, "SensorDynamics"), conc = conci, sdata = sdata.pulse, sensors = i)[, , 1]
        sdata.delta <- sdata.out - sdata.pulse
        sdatai <- sdatai + sdata.delta
        sdatai <- apply(sdatai, 1, sum)        

        # v2
        #sdatai <- predict(as(object, "SensorDynamics"), conc = conci, sdata = sdatai, sensors = i)
        #sdatai <- apply(sdatai, 1, sum)
      }
      
      sdatai
    }
    
    if(cores == 1) {
      sdata.out <- sapply(idx(object), run.sdata, simplify = FALSE)
    }
    else {
      sdata.out <- llply(idx(object), run.sdata, .parallel = TRUE)
    }
    
    stopifnot(length(sdata.out) == nsensors)
    for(i in 1:nsensors) {
      sdata[, i] <- sdata.out[[i]]
    }
  # case 2: ssd > 0  
  } else if (length(dim(coef)) == 3) { 
    if(dim(coef)[3] != nsensors)
      stop("Error in SensorModel::sdataModel: dim(coef)[3] != nsensors'.")    
    if(dim(coef)[2] != ncoef)
      stop("Error in SensorModel::sdataModel: 'dim(coef)[2] != ncoef'.")    
    
    sdata <- matrix(NA, nrow=n, ncol=nsensors)

    run.sdata <- function(i)
    {    
      conci <- conc[, , i]
      sdatai <- predict(object@dataModel[[i]], C=conc[, , i], B=coef[, , i], out = out, ...)  
      
      if(enableDyn) {
        # v1
        sdata.pulse <- sdata2pulse(object, conci, sdatai)
        sdata.out <- predict(as(object, "SensorDynamics"), conc = conci, sdata = sdata.pulse, sensors = i)[, , 1]
        sdata.delta <- sdata.out - sdata.pulse
        sdatai <- sdatai + sdata.delta
        sdatai <- apply(sdatai, 1, sum)        

        # v2
        #sdatai <- predict(as(object, "SensorDynamics"), conc = conci, sdata = sdatai, sensors = i)
        #sdatai <- apply(sdatai, 1, sum)
      }    
      
      sdatai
    }

    if(cores == 1) {
      sdata.out <- sapply(idx(object), run.sdata, simplify = FALSE)
    }
    else {
      sdata.out <- llply(idx(object), run.sdata, .parallel = TRUE) 
    }
          
    stopifnot(length(sdata.out) == nsensors)
    for(i in 1:nsensors) {
      sdata[, i] <- sdata.out[[i]]
    }  
  }
  else
    stop("Error in SensorModel::sdataModel: 'coef' dimenstion is unknown.")    

  if(cores > 1) {
    cat(" * Finished computing in parallel.\n")
  }
  
  return(sdata)
})
