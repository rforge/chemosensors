# last revision: 13 Jan 2012

#' @include SensorNoiseModelClass.R
NULL

#----------------------------
# Class constructor
#----------------------------

#' Function to get default constructor parameters of class \code{\link{SensorNoiseModel}}.
#' @rdname SensorNoiseModel-class
#' @aliases defaultSensorNoiseModel
#' @return List of the default parameters.
#' @export
defaultSensorNoiseModel <- function()
{
  par <- list(num=1, gases=1:3, gnames=LETTERS[1:3], concUnits="perc", concUnitsInt=defaultConcUnitsInt(),
    datasetSensorNoiseModel=defaultDataSensorNoiseModel(), pck=defaultDataPackage(), 
    ssd = 0.1, sntype = "randomWalk",  snf = c(1, 1, 1), sndata=NULL)
  
  return(par)
}

#' Constructor method of SensorNoiseModel Class.
#'
#' @name SensorNoiseModel
#' @rdname SensorNoiseModel-class
setMethod("initialize", "SensorNoiseModel", function(.Object,
  # common for sub-classes
  num="numeric", gases="numeric", gnames="character", concUnits="character", concUnitsInt="character",
  # specific for class SensorNoiseModel
  datasetSensorNoiseModel = "character", pck = "character",
  ssd = "numeric", sntype = "character", snf = "numeric", sndata, 
  # additional
  nsd = "numeric", ...)  
{   
  # missing
  def.par <- defaultSensorNoiseModel()

  if(missing(num)) num <- def.par$num  
  idx <- 1:length(num)
  
  if(missing(gases)) gases <- def.par$gases
  if(missing(gnames)) gnames <- def.par$gnames
  if(missing(concUnits)) concUnits <- def.par$concUnits
  if(missing(concUnitsInt)) concUnitsInt <- def.par$concUnitsInt

  if(missing(datasetSensorNoiseModel)) datasetSensorNoiseModel <- def.par$datasetSensorNoiseModel
  if(missing(pck)) pck <- def.par$pck
  if(missing(ssd)) ssd <- def.par$ssd
  if(missing(sntype)) sntype <- def.par$sntype
  if(missing(snf)) snf <- def.par$snf
  if(missing(sndata)) sndata <- def.par$sndata
  
  if(!missing(nsd)) ssd <- nsd
    
  # load data
  if(is.null(sndata)) {
    #data(list=datasetSensorNoiseModel, package=pck, envir=environment()) # -> 'Bsd'
    #if(!(exists("Bsd")))
    #  stop("Error in SensorNoiseModel::initialize: 'datasetSensorNoiseModel' is not loaded; variable 'Bsd' is not found.")    
    
    #if(!exists(datasetSensorNoiseModel)) # datasetSensorModel supposed to be `UNIMANsnoise`
    #  stop("Error in SensorNoiseModel::initialize: dataset", datasetSensorNoiseModel, "is not loaded.")    
    #eval(parse(text = paste("mdat <-", datasetSensorNoiseModel)))
    
    mdat <- loadUNIMANdata(datasetSensorNoiseModel)
    
    Bsd <- mdat$Bsd
    
    sndata <- Bsd[["SensorModel"]][["plsr"]] # defaults
  }      

  # check 'num'
  if(sum(num <= 0 | num > ncol(sndata)))
    stop("Error in SensorNoiseModel::initialize: 'num' is incorrect.")  
  
  # filter by 'gases'
  ngases <- length(gases)
  if(length(gnames) != ngases) gnames <- gnames[gases]

  # NOT SUPPORTED: ssd
  if(length(ssd) > 1) {
    stop("Error in SensorNoiseModel::initialize: case 'length(ssd) > 1' not supported yet.")      
    #if(length(ssd) != ngases) ssd <- ssd[gases]
  }

  # filter 'sndata' by 'gases'
  ngases0 <- 3
  ncoef0 <- nrow(sndata) / ngases0
  coefi <- sort(as.numeric(sapply(gases, function(x) seq(x, by=ngases0, length=ncoef0))))
  
  sndata <- sndata[coefi, ]
  if(is.null(nrow(sndata))) sndata <- matrix(sndata, nrow=1) # 1-nrow case

  # filter 'sndata' by 'num'
  sndata <- sndata[, num]
  if(is.null(ncol(sndata))) sndata <- matrix(sndata, ncol=1) # 1-col case

  # set names of 'sndata'
  colnames(sndata) <- num
  #rownames(sndata) <- rep(gnames, ncoef0)

  # filter 'snf' by 'gases' (after 'sndata' is filtered)
  if(length(snf) != ngases) snf <- snf[gases]
  ncoef0 <- nrow(sndata) / ngases
  snf <- rep(snf, each=ncoef0)
  
  # assign
  .Object@idx <- idx
  .Object@num <- num
  .Object@gases <- gases
  .Object@ngases <- ngases
  .Object@gnames <- gnames
  .Object@concUnits <- concUnits
  .Object@concUnitsInt <- concUnitsInt

  .Object@ssd <- ssd  
  .Object@sntype <- sntype
  .Object@snf <- snf
  .Object@sndata <- sndata
    
  validObject(.Object)
  return(.Object)
})

#' Wrapper function SensorNoiseModel.
#'
#' @name SensorNoiseModel
#' @rdname SensorNoiseModel-class
#' @param ... parameters of constructor.
#' @export
SensorNoiseModel <- function(...)
{
  new("SensorNoiseModel", ...)
}

#----------------------------
# Plot Methods
#----------------------------

#' @rdname plot-methods
#' @aliases plot,SensorNoiseModel-method
setMethod("plot", "SensorNoiseModel", function (x, y, ...) 
{
  yval <- c("noise", "walk", "barplot")
  # missing
  if(missing(y)) y <- "barplot"
  
  switch(y,
    barplot = plot.SensorNoiseModel.barplot(x, y, ...),
    noise = plot.SensorNoiseModel.noise(x, y, ...),
    walk = plot.SensorNoiseModel.walk(x, y, ...),
    stop("Error in SensorNoiseModel::plot: plot type 'y' is unknown. Supported types: ", 
      paste(yval, collapse=", "), "."))
})

plot.SensorNoiseModel.barplot <- function(x, y, 
  main = "Sensor Noise",  
  xlab = "Gases", ylab="Parameter 'sd'", ...)
{
  nsensors <- nsensors(x)
  
  # main
  if(missing(main)) {
    if(nsensors <= 5) main <- paste(main, ": num ", paste(num(x), collapse=", "), sep='')
    else main <- paste(main, ": ", nsensors, " sensors", sep='')
    main <- paste(main, ", ssd ", ssd(x), sep='')
  }
  
  barplot(t(x@sndata), beside=TRUE, 
    main=main, xlab=xlab, ylab=ylab, ...)
}

plot.SensorNoiseModel.noise <- function(x, y, n, coef, 
  lwd = 2, lty = 1,
  main = paste("Sensor Noise: noise '", type(x), "', ssd ", ssd(x), sep=''), 
  xlab = "Samples", ylab="Coefficients", ...)
{
  if(missing(coef)) coef <- coefSample(x)
  if(missing(n)) n <- 100
  
  ncoef <- predict(x, coef=coef, n=n, ...) # noise + coef
   
  col <- grey.colors(ncoef(x), start=0.3, end=0.7) 
  
  matplot(ncoef, type = 'l', col=col, lwd = lwd, lty = lty,
    bty='n',
    main=main, xlab = xlab, ylab = ylab, ...)  
}

plot.SensorNoiseModel.walk <- function(x, y, n = 100, k = 5, coef, coefind = 1:2,
  pch = 20,
  main = paste("Sensor Noise Walk: ssd ", ssd(x), ", k ", k, sep=''), 
  xlab = "Coefficient", ylab="Coefficient", ...)
{
  if(missing(coef)) coef <- coefSample(x)
  
  nsensors <- nsensors(x)
  
  coef <- predict(x, coef=coef, n=n*k) # noise + coef
  coef <- switch(nsensors, 
    '1' = coef[, coefind], 
    coef[, coefind, ])    
  coefnames <- coefnames(x)[coefind]
  
  group <- rep(1:k, each=n)
  
  # plot param
  gcol <- grey.colors(k, start=0.2, end=0.8)[k:1]
  col <- gcol[group]
  
  xlab <- paste(xlab, coefnames[1])
  ylab <- paste(ylab, coefnames[2])
    
  # plot
  if(nsensors == 1) {
    plot(coef, t='p', bty='n', col=col, pch=pch,
      main=main, xlab = xlab, ylab = ylab, ...)  
  
    theta <- seq(0, 2*pi, length=1000)
    for(i in 1:k) {
      meani <- apply(coef[group <= i, ], 2, mean)
      sdi <- apply(coef[group <= i, ], 2, sd)  
      
      xp <- meani[1] + sdi[1] * cos(theta)
      yp <- meani[2] + sdi[2] * sin(theta)    
      lines(xp, yp, col=gcol[i])
    }
  }
  else {    
    for(i in 1:nsensors) {
      coefi <- coef[, , i]
      xlabi <- paste(xlab, "(", num(x)[i], ")")
      ylabi <- paste(ylab, "(", num(x)[i], ")")    

      plot(coefi, t='p', bty='n', col=col, pch=pch,
        main=main, xlab = xlabi, ylab = ylabi, ...)  
  
      theta <- seq(0, 2*pi, length=1000)
      for(i in 1:k) {
        meani <- apply(coefi[group <= i, ], 2, mean)
        sdi <- apply(coefi[group <= i, ], 2, sd)  
        
        xp <- meani[1] + sdi[1] * cos(theta)
        yp <- meani[2] + sdi[2] * sin(theta)    
        lines(xp, yp, col=gcol[i])
      }    
    }
  }
}

#----------------------------
# Get/Set Methods
#----------------------------

#' @rdname get-methods
#' @aliases ncoef,SensorNoiseModel-method
setMethod("ncoef", "SensorNoiseModel", function(x) nrow(x@sndata))

#' @rdname get-methods
#' @aliases coefnames,SensorNoiseModel-method
setMethod("coefnames", "SensorNoiseModel", function(x) 
{
  ncoef <- ncoef(x)
  ngases <- ngases(x)
  
  coefnames <- as.character(1:ncoef)
  if(ncoef == ngases) {
    coefnames <- gnames(x)
  }  
  #else
  #  stop("Error in SensorNoiseModel::coefnames: case 'ncoef != ngases' is not supported.")

  return(coefnames)
})

#----------------------------
# Noise Methods
#----------------------------

### Method scaledNoiseSd
setMethod("scaledNoiseSd", "SensorNoiseModel", function(object, ...)
{
  scaledSd <- switch(type(object),
    #randomWalk = 3 * ssd(object), # 0.7.4
    randomWalk = 3 * ssd(object), # 0.7.5
    stop("Error in SensorNoiseModel::scaledNoiseSd: 'type' is unknown."))

  return(scaledSd)
})


#----------------------------
# Predict Methods
#----------------------------

#' @rdname model-methods
#' @aliases predict,SensorNoiseModel-method
setMethod ("predict", "SensorNoiseModel", function(object, coef, n, ...)
{ 
  if(missing(coef)) 
      stop("Error in SensorNoiseModel::predict: 'coef' is missing.")
  if(missing(n)) 
      stop("Error in SensorNoiseModel::predict: 'n' is missing.")
                
  ncoef <- ncoef(object)  
  nsensors <- nsensors(object)  
  
  # pre-process 'ssd'
  ssd <- scaledNoiseSd(object)  
  ssd <- ssd / sqrt(n) # random walk: sigma(n) = sd / sqrt(n)

  if(ssd) {
    Bsd <- object@sndata
    Bsd <- ssd * Bsd * object@snf # scale by factor 

    N <- switch(type(object),
      "randomWalk" = {
        apply(Bsd, 1:2, function(x, n) {
          rnorm(n, sd = x)
        }, n)
      },
      stop("Error in SensorNoiseModel::predict: 'type' is unknown."))
  }
  
  B.list <- llply(1:nsensors, function(i) {
    # all matricies here 
    # - matrix of coefficients `B`;
    # - matricies of noise in coefficients `N.step` and `N.stepsum`;
    # have dimensions of #rows = n, #cols = ncoef
    coefi <- coef[, i]
    B <- matrix(coefi, nrow = n, ncol = ncoef, byrow = TRUE)
    
    if(ssd) {
      Bsd <- object@sndata
      Bsd <- ssd * Bsd * object@snf # scale by factor 
      
      N.step <-  N[, , i] # noise at each sample from 1 to n
      N.stepsum <- apply(N.step, 2, cumsum) # random-walk noise 
      
      B <- B + N.stepsum
    }
    
    B
  })
  
  # output array `B` of three dimensions
  # - dim 1: number of samples `n`
  # - dim 2: number of coefficients `ncoef`
  # - dim 3: number of sensors `nsensors`
  B <- array(NA, c(n, ncoef, nsensors))
  
  for(i in 1:nsensors) {
    B[, , i] <- B.list[[i]]
  }
  
  dimnames(B) <- list("1" = list(), "2"= coefnames(object), "3" = dimnames(object@sndata)[[2]])
  
  ### output
  if(nsensors == 1) {
    B <- B[, , 1, drop = TRUE]
  }

  return(B)
})

#----------------------------
# Model Methods
#----------------------------

setMethod("coefSample", "SensorNoiseModel", function(object, ...)
{
  ncoef <- ncoef(object)
  gases <- gases(object) 
  ngases <- ngases(object)
  nsensors <- nsensors(object)
  
  if(ncoef == ngases) {
    coefval <- c(0.1, 0.1, 0.01) # coefficient for num = 1 (data model plsr)
    coefval <- coefval[gases]
    
    coef <- matrix(coefval, nrow=ngases, ncol=nsensors)

    # set names of 'coef'
    colnames(coef) <- num(object)
    rownames(coef) <- gnames(object)
  }  
  else
    stop("Error in SensorNoiseModel::coefSample: case 'ncoef != ngases' is not supported.")
  
  return(coef)
})



