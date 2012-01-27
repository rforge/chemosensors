# last revision: 13 Jan 2012

#' @include SensorNoiseModelClass.R
NULL

#----------------------------
# Class constructor
#----------------------------

#' Get default constructor parameters of class \code{\link{SensorNoiseModel}}.
#' @name defaultSensorNoiseModel
#' @rdname pub-defaultSensorNoiseModel
#' @keywords SensorNoiseModel defaults
#' @return List of the default parameters.
#' @export
defaultSensorNoiseModel <- function()
{
  par <- list(num=1, gases=1:3, gnames=LETTERS[1:3], concUnits="perc", concUnitsInt=defaultConcUnitsInt(),
    datasetSensorNoiseModel=defaultDataSensorNoiseModel(), pck=defaultDataPackage(), 
    ssd = 0.1, sntype = "randomWalk",  snf = c(1, 1, 0.2), sndata=NULL)
  
  return(par)
}

### Constructor of SensorNoiseModel class.
setMethod("initialize", "SensorNoiseModel", function(.Object,
  # common for sub-classes
  num="numeric", gases="numeric", gnames="character", concUnits="character", concUnitsInt="character",
  # specific for class SensorNoiseModel
  datasetSensorNoiseModel = "character", pck = "character",
  ssd = "numeric", sntype = "character", snf = "numeric", sndata, ...)
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
  
  # load data
  if(is.null(sndata)) {
    data(list=datasetSensorNoiseModel, package=pck, envir=environment()) # -> 'Bsd'
    if(!(exists("Bsd")))
      stop("Error in SensorNoiseModel::initialize: 'datasetSensorNoiseModel' is not loaded; variable 'Bsd' is not found.")    
    sndata <- Bsd[["SensorModel"]][["plsr"]] # defaults
  }      

  # check 'num'
  if(sum(num <= 0 | num > ncol(sndata)))
    stop("Error in SensorModel::initialize: 'num' is incorrect.")  
  
  # filter by 'gases'
  ngases <- length(gases)
  if(length(gnames) != ngases) gnames <- gnames[gases]
  if(length(snf) != ngases) snf <- snf[gases]    

  # NOT SUPPORTED: ssd
  if(length(ssd) > 1) {
    stop("Error in SensorNoiseModel::initialize: case 'length(ssd) > 1' not supported yet.")      
    #if(length(ssd) != ngases) ssd <- ssd[gases]
  }
  
  # filter 'sndata' by 'gases'
    
  sndata <- sndata[gases, ]
  if(is.null(nrow(sndata))) sndata <- matrix(sndata, nrow=1) # 1-nrow case

  # filter 'sndata' by 'num'
  sndata <- sndata[, num]
  if(is.null(ncol(sndata))) sndata <- matrix(sndata, ncol=1) # 1-nrow case

  # set names of 'sndata'
  colnames(sndata) <- num
  rownames(sndata) <- gnames
  
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

#' @export
SensorNoiseModel <- function(...)
{
  new("SensorNoiseModel", ...)
}

#----------------------------
# Plot Methods
#----------------------------
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
  
  matplot(ncoef, t='l', col=col, lwd = lwd, lty = lty,
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

setMethod("ncoef", "SensorNoiseModel", function(x) nrow(x@sndata))

setMethod("coefnames", "SensorNoiseModel", function(x) 
{
  ncoef <- ncoef(x)
  ngases <- ngases(x)
  
  coefnames <- as.character(1:ncoef)
  if(ncoef == ngases) {
    coefnames <- gnames(x)
  }  
  else
    stop("Error in SensorNoiseModel::coefnames: case 'ncoef != ngases' is not supported.")

  return(coefnames)
})

#----------------------------
# Noise Methods
#----------------------------

### Method scaledNoiseSd
setMethod("scaledNoiseSd", "SensorNoiseModel", function(object, ...)
{
  scaledSd <- switch(type(object),
    randomWalk = ssd(object),
    stop("Error in SensorNoiseModel::scaledNoiseSd: 'type' is unknown."))

  return(scaledSd)
})


#----------------------------
# Predict Methods
#----------------------------

setMethod ("predict", "SensorNoiseModel", function(object, coef, n, ...)
{ 
  if(missing(coef)) 
      stop("Error in SensorNoiseModel::predict: 'coef' is missing.")
  if(missing(n)) 
      stop("Error in SensorNoiseModel::predict: 'n' is missing.")
                
  ncoef <- ncoef(object)  
  nsensors <- nsensors(object)  
  
  #if(length(coef) != ncoef)
  #  stop("Error in SensorNoiseModel::predict: length 'coef' is incorrect.")      
  
  ssd <- scaledNoiseSd(object)
  nsensors.str <- ifelse(nsensors == 1, "one", "many")
  
  B <- switch(nsensors.str,
    one = array(coef, c(ncoef, n)),
    many= array(coef, c(ncoef, nsensors, n)))    
  
  if(ssd) {
    # add noise  
    Bsd <- object@sndata
    Bsd <- ssd * Bsd * object@snf # scale by factor 

    if(type(object) == "randomWalk") {
     N <- switch(nsensors.str,
        one = aaply(Bsd, 1, function(x, n) rnorm(n, sd=x), n),
        many = aaply(Bsd, c(1, 2), function(x, n) rnorm(n, sd=x), n))

      N <- switch(nsensors.str,
        one = aaply(N, 1, cumsum),
        many = aaply(N, c(1, 2), cumsum)) 
         
      B <- B + N
    }
    else
      stop("Error in SensorNoiseModel::predict: 'type' is unknown.")        
  }

  # transpose 'B'  
  B <- switch(nsensors.str,
    one = t(B),
    many = aaply(B, 3, function(x) x))

  # names
  dimnames(B) <- switch(nsensors.str,
    one = list("1"=list(), "2"=coefnames(object)),
    many = list("1"=list(), "2"=coefnames(object), "3"=dimnames(object@sndata)[[2]]))

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



