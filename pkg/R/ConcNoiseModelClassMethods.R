# last revision: 12 Jan 2012

#' @include ConcNoiseModelClass.R
NULL

#----------------------------
# Class constructor
#----------------------------

#' Get default constructor parameters of class \code{\link{ConcNoiseModel}}.
#' @rdname ConcNoiseModel-class
#' @aliases defaultParConcNoiseModel
#' @return List of the default parameters.
#' @export
defaultParConcNoiseModel <- function()
{
  par <- list(gases=1:3, gnames=LETTERS[1:3], concUnits="perc", concUnitsInt=defaultConcUnitsInt(),
    csd = 0.1, cntype = "logconc",  cnlogf = c(1, 1, 2))
  
  return(par)
}

#' Constructor method of ConcNoiseModel Class.
#'
#' @name ConcNoiseModel
#' @rdname ConcNoiseModel-class
setMethod("initialize", "ConcNoiseModel", function(.Object,
  # common for sub-classes
  gases="numeric", gnames="character", concUnits="character", concUnitsInt="character",
  # specific for class ConcNoiseModel
  csd = "numeric", cntype = "character", cnlogf = "numeric", 
  # additional
  nsd = "numeric", ...)
{   
  # missing
  def.par <- defaultParConcNoiseModel()
  
  if(missing(gases)) gases <- def.par$gases
  if(missing(gnames)) gnames <- def.par$gnames
  if(missing(concUnits)) concUnits <- def.par$concUnits
  if(missing(concUnitsInt)) concUnitsInt <- def.par$concUnitsInt

  if(missing(csd)) csd <- def.par$csd
  if(missing(cntype)) cntype <- def.par$cntype
  if(missing(cnlogf)) cnlogf <- def.par$cnlogf
         
  if(!missing(nsd)) csd <- nsd
           
  # filter by 'gases'
  ngases <- length(gases)
  if(length(gnames) != ngases) gnames <- gnames[gases]
  gind <- rep(NA, max(gases))
  for(i in 1:length(gases)) {
    g <- gases[i]
    gind[g] <- i
  }

  if(length(cnlogf) != ngases) cnlogf <- cnlogf[gases]    
  if(length(csd) > 1) {
    if(length(csd) != ngases) csd <- csd[gases]
  }
      
  # assign
  .Object@gases <- gases
  .Object@gind <- gind  
  .Object@ngases <- ngases
  .Object@gnames <- gnames
  .Object@concUnits <- concUnits
  .Object@concUnitsInt <- concUnitsInt

  .Object@csd <- csd  
  .Object@cntype <- cntype
  .Object@cnlogf <- cnlogf
  
  validObject(.Object)
  return(.Object)
})

#' Wrapper function ConcNoiseModel.
#'
#' @name ConcNoiseModel
#' @rdname ConcNoiseModel-class
#' @param ... parameters of constructor.
#' @export
ConcNoiseModel <- function(...)
{
  new("ConcNoiseModel", ...)
}

#----------------------------
# Plot Methods
#----------------------------

#' @rdname plot-methods
#' @aliases plot,ConcNoiseModel-method
setMethod("plot", "ConcNoiseModel", function (x, y, ...) 
{
  # missing
  if(missing(y)) y <- "noise"
  
  switch(y,
    noise = plot.ConcNoiseModel.noise(x, y, ...),
    stop("Error in ConcNoiseModel::plot: plot type 'y' is unknown."))
})

plot.ConcNoiseModel.noise <- function(x, y, n = 100, conc, concUnits="default",
  col, lty = c(3, 1), lwd = 2,
  main = paste("Concentration Noise: csd", csd(x)), xlab = "Samples", ylab="Concentration", ...)
{
  if(concUnits == "default") concUnits <- concUnits(x)
  if(missing(conc)) conc <- concSample(x, n=n, concUnits=concUnits, ...)
  
  if(missing(col)) col <- gcol(x)
  
  ngases <- ngases(x)
  nconc <- predict(x, conc, ...)  

  lty <- rep(lty, each=ngases)
  ylab <- paste(ylab, ", [", ConcUnitsStr(concUnits), "]", sep="")
  matplot(cbind(conc, nconc), type = 'l', col=col, lwd = lwd, lty = lty,
    bty='n',
    main=main, xlab = xlab, ylab = ylab)  
}

#----------------------------
# Get/Set Methods
#----------------------------

#----------------------------
# Noise Methods
#----------------------------

### Method scaledNoiseSd
setMethod("scaledNoiseSd", "ConcNoiseModel", function(object, ...)
{
  scaledSd <- switch(type(object),
    logconc = csd(object),
    stop("Error in ConcNoiseModel::scaledNoiseSd: 'type' is unknown."))

  return(scaledSd)
})


#----------------------------
# Predict Methods
#----------------------------

#' @rdname model-methods
#' @aliases predict,ConcNoiseModel-method
setMethod ("predict", "ConcNoiseModel", function(object, conc, concUnits="default", ...)
{  
  if(concUnits == "default") concUnits <- concUnits(object)
  
  if(is.null(ncol(conc))) conc <- matrix(conc, ncol=1) # 1-column case
  if(ncol(conc) != ngases(object))
    stop("Error in ConcNoiseModel::predict: dimension of 'conc' is incorrect.")  
  
  if(concUnits != concUnitsInt(object)) conc <- concNorm(object, conc, concUnits) # concNorm
  conc <- concModel(object, conc=conc, concUnits=concUnitsInt(object), ...)  # concModel

  if(concUnitsInt(object) != concUnits) conc <- concDenorm(object, conc, concUnits) # concDenorm
  
  return(conc)
})

#----------------------------
# Model Methods
#----------------------------

setMethod("concModel", "ConcNoiseModel", function(object, conc, concUnits="default", ...)
{
  if(concUnits == "default") concUnits <- concUnits(object)
  if(missing(conc)) 
    stop("Error in ConcNoiseModel::predict: 'conc' is missing.")
    
  if(concUnitsInt(object) != concUnits)
    stop("Error in ConcNoiseModel::concModel: 'concUnits' is different from slot 'concUnitsInt'.")    
  

  C <- as.matrix(conc)
  n <- nrow(C)
 
  # pre-process 'csd'  
  csd <- scaledNoiseSd(object)
    
  N <- matrix(0, nrow=nrow(C), ncol=ncol(C)) # case 'csd == 0'
  if(type(object) == "logconc") {
    
    logC <- log(1 + C)
    logC <- t(t(logC) * logf(object)) # t() is used to multiply by columns
  
    N <- 0.5 * matrix(rnorm(prod(dim(C))), n, ngases(object))
    N <- t(t(N) * csd) # t() is used to multiply by columns
    
    C <- C + logC * N
  }
  else 
    stop("Error in ConcNoiseModel::concModel: 'type' is unknown.")

  return(C)  
})
