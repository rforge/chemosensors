# last revision: 9 Jan 2012

#' @include ChemosensorsClass.R
NULL

#----------------------------
# Sub Classes
#----------------------------

#----------------------------
# Parameters
#----------------------------

#' Get names of concentration units.
#' @name ConcUnitsNames
#' @rdname pub-ConcUnitsNames
#' @keywords parameter
#' @return Character vector of units names.
#' @export
ConcUnitsNames <- function()
{
  return(c("perc", "perc 1e-2", "norm"))
}

#' @export
ConcUnitsStr <- function(units)
{
  unitsStr <- switch(units,
    "perc" = "vol. %",
    "perc 1e-2" = "1e-2 vol. %",
    stop("Error in concUnitsStr: 'units' is unknown."))

  return(unitsStr)
}

#----------------------------
# Defaults 
#----------------------------

#' @export
defaultDataModel <- function() return("mvr")

#' @export
defaultConcUnitsInt <- function() return("perc 1e-2")

#' @export
defaultConcUnits <- function() return("perc")

#' @export
defaultConcUnitsSorption <- function() return("norm")

#----------------------------
# Defaults (Datasets)
#----------------------------

#' @export
defaultDataPackage <- function() return("chemosensors")

#' @export
defaultDataSensorModel <- function() return("UNIMANshort")

#' @export
defaultDataSensorNoiseModel <- function() return("UNIMANsnoise")

#' @export
defaultDataSorptionModel <- function() return("UNIMANsorption")

#' @export
defaultDataDriftNoiseModel <- function() return("UNIMANdnoise")

#----------------------------
# Get/Set Methods
#----------------------------

setMethod("num", "ANY", function(x) x@num)
setMethod("idx", "ANY", function(x) x@idx)

setMethod("gases", "ANY", function(x) x@gases)
setMethod("gind", "ANY", function(x) x@gind)
setMethod("ngases", "ANY", function(x) x@ngases)
setMethod("gnames", "ANY", function(x) x@gnames)

setMethod("gases", "missing", function(x) 1:3)
setMethod("gind", "missing", function(x) 1:3)
setMethod("ngases", "missing", function(x) 3)
setMethod("gnames", "missing", function(x) LETTERS[1:3])

setMethod("nsensors", "ANY", function(x) length(x@num))

setMethod("concUnits", "ANY", function(x) x@concUnits)
setMethod("concUnitsInt", "ANY", function(x) x@concUnitsInt)

setReplaceMethod("concUnits", "ANY", function(object, value) 
{
  object@concUnits <- value
  validObject(object)
  return (object)
})

#----------------------------
# Predict Methods
#----------------------------

### Method coef
setMethod("coef", "ANY", function(object, ...)
{
  coefficients(object, ...)  
})


#----------------------------
# Model Methods
#----------------------------
setMethod("concMin", "ANY", function(object, concUnits=object@concUnits, ...)
{
  conc.min.perc <- c(0.01, 0.01, 0.1)[gases(object)]
  conc.min <- switch(concUnits,
    "perc" = conc.min.perc,
    "perc 1e-2" = 100 * conc.min.perc,
    "norm" = conc.min.perc / concMax(object, concUnits="perc"),  
    stop("Error in ANY::concMin: 'concUnits' is not supported."))
  
  return(conc.min)
})

setMethod("concMax", "ANY", function(object, concUnits=object@concUnits, ...)
{
  conc.max.perc <- c(0.05, 0.05, 1.0)[gases(object)]
  conc.max <- switch(concUnits,
    "perc" = conc.max.perc,
    "perc 1e-2" = 100 * conc.max.perc,
    "norm" = c(1, 1, 1)[gases(object)],
    stop("Error in ANY::concMax: 'concUnits' is not supported."))
  
  return(conc.max)
})


### Method concNorm
setMethod("concNorm", "ANY", function(object, conc, concUnits="default", concUnitsInt="default", ...)
{
  if(missing(conc)) 
    stop("Error in ANY::concNorm: missing 'conc'.")  
  if(concUnits == "default") concUnits <- concUnits(object)
  if(concUnitsInt == "default") concUnitsInt <- concUnitsInt(object)

  if(!(concUnits %in% ConcUnitsNames())) # from 'ChemosensorsClassMethods.R'
    stop("Error in ANY::concNorm: 'concUnits' is incorrect.")
  
  conc <- switch(concUnitsInt,
     "perc 1e-2" = switch(concUnits,
        "perc" = 100 * conc,
        "perc 1e-2" = conc,        
        stop("Error in ANY::concNorm: 'concUnits' is not supported for 'concUnitsInt' equal to 'perc 1e-2'.")),
     "norm" = sweep(conc, 2, concMax(object, concUnits=concUnits), "/"),
     stop("Error in ANY::concNorm: 'concUnitsInt' is not supported."))
    
  return(conc)
})

### Method concNorm
setMethod("concNorm", "missing", function(object, conc, concUnits, concUnitsInt, ...)
{
  if(missing(conc)) 
    stop("Error in missing::concNorm: missing 'conc'.")  
  if(missing(concUnits)) concUnits <- defaultConcUnits()
  if(missing(concUnitsInt)) concUnitsInt <- defaultConcUnitsInt()
  
  conc <- switch(concUnitsInt,
     "perc 1e-2" = switch(concUnits,
        "perc" = 100 * conc,
        "perc 1e-2" = conc,        
        stop("Error in ANY::concNorm: 'concUnits' is not supported for 'concUnitsInt' equal to 'perc 1e-2'.")),
     "norm" = sweep(conc, 2, concMax(object, concUnits=concUnits), "/"),
     stop("Error in ANY::concNorm: slot 'concUnitsInt' is not supported."))
         
  return(conc)
})

### Method concDenorm
setMethod("concDenorm", "ANY", function(object, conc, concUnits="default", concUnitsInt="default", ...)
{
  if(missing(conc)) 
    stop("Error in ANY::concDenorm: missing 'conc'.")  
  if(concUnits == "default") concUnits <- concUnits(object)
  if(concUnitsInt == "default") concUnitsInt <- concUnitsInt(object)  
  
  if(!(concUnits %in% ConcUnitsNames())) # from 'ChemosensorsClassMethods.R'
    stop("Error in ANY::concDenorm: 'concUnits' is incorrect.")
  
  conc <- switch(concUnits,
     "perc" = switch(concUnitsInt,
        "perc 1e-2" = 0.01 * conc,
        "perc" = conc,
        "norm" = sweep(conc, 2, concMax(object, concUnits=concUnits), "*"),   
        stop("Error in ANY::concDenorm: 'concUnitsInt' (", concUnitsInt, ") is not supported for 'concUnits' equal to 'perc'.")),
     "perc 1e-2" = switch(concUnitsInt,
        "perc 1e-2" = conc,
        "perc" = 100 * conc,
        "norm" = sweep(conc, 2, concMax(object, concUnits=concUnits), "*"),   
        stop("Error in ANY::concDenorm: 'concUnitsInt' (", concUnitsInt, ") is not supported for 'concUnits' equal to 'perc'.")),
     stop("Error in ANY::concDenorm: 'concUnits' (", concUnits, ") is not supported."))
    
  return(conc)
})

### Method concSample
setMethod("concSample", "ANY", function(object, type, n, 
  gases="default", concUnits="default", ...)
{
  tval <- c("const", "inc", "range")
  
  if(missing(n)) n <- 100
  if(missing(type)) type <- "const"

  if(gases == "default") gases <- gases(object)
  if(concUnits == "default") concUnits <- concUnits(object)

  ngases <- ngases(object) 
  gind <- gind(object)
  
  # create 'conc' ('perc' units)
  # n samples per gas
  m <- concMin(object, concUnits=concUnits)
  M <- concMax(object, concUnits=concUnits)
  cgases <- c(0.02, 0.02, 0.2) 
  cgases <- cgases[gases]

  if(type == "const") {
    conc <- matrix(0, nrow= n * length(gases), ncol=ngases) 
    for(i in 1:length(gases)) {
      g <- gases[i] 
      gi <- gind[g] 
      ind <- 1:n + (i - 1) * n
      conc[ind, gind[g]] <- cgases[gi]
    }
  }
  else if(type == "inc") {
    conc <- matrix(0, nrow= n * length(gases), ncol=ngases) 
    for(i in 1:length(gases)) {
      g <- gases[i] 
      gi <- gind[g]   
      ind <- 1:n + (i - 1) * n
      conc[ind, gind[g]] <- seq(0, M[gi], length=n)
    }
  }
  else if(type == "range") {
    conc <- matrix(0, nrow= n * length(gases), ncol=ngases) 
    for(i in 1:length(gases)) {
      g <- gases[i] 
      gi <- gind[g] 
      ind <- 1:n + (i - 1) * n
      conc[ind, gind[g]] <- seq(m[gi], M[gi], length=n)
    }
  }  
  else 
    stop("Error in ANY::concSample: 'type' is unknown.")
  
  # convert 'conc' to '@concUnits'
  #conc <- switch(concUnits,
  #  'perc' = conc,
  #  'perc 1e-2' = 100 * conc,
  #   'norm' = sweep(conc, 2, concMax(object, concUnits=concUnits), "/"),    
  #  stop("Error in ANY::concSample: 'concUnits' is not supported."))
    
  return(conc)
})

setMethod("sdataSample", "ANY", function(object, n, ...)
{
  if(missing(n)) n <- 100
  nsensors <- nsensors(object)
  
  sdata.base <- matrix(seq(0, nsensors, length=nsensors), nrow=n, ncol=nsensors, byrow=TRUE)
  sdata.inc <- matrix(seq(2, 5, length=n), nrow=n, ncol=nsensors)
  sdata <- sdata.inc # + sdata.base

  # set names of 'coef'
  colnames(sdata) <- num(object)
  
  return(sdata)
})


#----------------------------
# Noise Methods
#----------------------------

setReplaceMethod("nsd", "ANY", function(object, value) 
{
  return (object)
})

#----------------------------
# Plot Methods
#----------------------------

### Method plotResponse
setMethod("plotResponse", "ANY", function(x, y, jitter, 
  gases = 0, type = "range", n = 100, 
  uniman=FALSE, datasetSensorModel, pck, 
  lwd = 1, lty = 1, pch=20, 
  xlim = c(0, 1.2), ylim, 
  main = "Model Response", xlab = "Normalized Concentration", ylab="Signal", ...)
{
  if(uniman) {
    if(missing(datasetSensorModel)) datasetSensorModel <- defaultDataSensorModel()
    if(missing(pck)) pck <- defaultDataPackage()
    if(missing(jitter)) jitter <- TRUE    
  }  
  if(missing(jitter)) jitter <- FALSE
  if(sum(gases == 0)) gases <- gases(x)
  if(missing(ylim)) ylim <- NULL
      
  concUnits <- concUnits(x)  
  gind <- gind(x)
  nsensors <- nsensors(x)
  
  nsd(x) <- 0
    
  # points
  yp <- matrix(NA, nrow=n, ncol=length(gases) * nsensors)
  xp <- matrix(NA, nrow=n, ncol=length(gases) * nsensors)
  for(i in 1:length(gases)) {
    g <- gases[i]  
    gi <- gind[g]
    
    conci <- concSample(x, type, n=n, gases=g, concUnits=concUnits)
    
    ind <- seq(1, nsensors) + (i - 1) * nsensors
    xpi <- concNorm(x, conc=conci, concUnits=concUnits, concUnitsInt="norm")[, gi]
    xp[, ind] <- xpi

    ypi <- predict(x, conc=conci)
    yp[, ind] <- ypi
  }
  
  # ylim
  if(uniman) {
    data(list=datasetSensorModel, package=pck, envir=environment()) # -> 'C', 'dat', 'dat.corrected'
    X <- dat

    if(is.null(ylim)) ylim <- range(range(X[, num(x)]), range(yp))
  }  
  
  col <- gcol(x, gases=rep(gases, each=nsensors))  
  matplot(xp, yp, bty='n', t='l', col=col, 
    lwd = lwd, lty = lty,
    xlim = xlim, ylim=ylim, 
    main=main, xlab = xlab, ylab = ylab, ...)  
    
  if(uniman) {
    num <- num(x)
        
    for(i in 1:length(gases)) {
      g <- gases[i]
      gi <- gind[g]
      ind <- (C[, g] != 0)
      
      xpi <- concNorm(x, conc=C[ind, gases], concUnits=concUnits, concUnitsInt="norm")[, gi]
      xpi <- matrix(xpi, nrow=length(xpi), ncol=nsensors)
      ypi <- X[ind, num]
      
      if(jitter) xpi <- jitter(xpi, factor=0.2)  
      
      col <- gcol(x, gases=g)
      points(xpi, ypi, col=col, pch=pch)
    }
  }
})

#----------------------------
# Color Methods
#----------------------------
ccol.function <- function(conc, pal,
  gases, ngases, ...)
{
  nsamples <- nrow(conc)

  # cmode
  all.pure <- (sum(apply(conc, 1, function(x) sum(x!=0) <= 1)) == nsamples)
  cmode <- ifelse(all.pure, 'pures', 'mixtures')
  
  col <- rep("black", nsamples)
  #if(cmode == 'pures') {
    for(i in 1:ngases) {
      g <- gases[i]
      ind <- which(apply(conc, 1, function(x) sum(x[-i]) == 0)) # pure gas i
      conci <- conc[, i]
      
      pali <- pal[[i]]
      col[ind] <- pali[4]
    }
  #}
  #else {
  #  warning("Error in 'ANY::ccol': 'cmode' is unknown.")        
  #}

  return(col)  
}
  
### Method ccol
setMethod("ccol", "ANY", function(object, conc, pal, ...)
{ 
  # missing object/conc
  missing.object <- ifelse(missing(object), TRUE, FALSE) 
  missing.conc <- ifelse(missing(conc), TRUE, FALSE) 

  if(!missing.object) {
    if(class(object) %in% c("matrix", "data.frame")) {
      missing.object <- TRUE
      missing.conc <- FALSE
      conc <- object
    }
  }
  
  if(missing.conc)
    stop("Error in 'ANY::ccol': 'conc' is missing.")        
      
  palA <- brewer.pal(4, "Blues")
  palB <- brewer.pal(4, "Reds")
  palC <- brewer.pal(4, "Greens")
  
  pal <- list(A=palA, B=palB, C=palC)

  gases <- ifelse(missing.object, gases(), gases(object))
  ngases <- ifelse(missing.object, ngases(), ngases(object))  
  
  col <- ccol.function(conc, pal, 
    gases=gases, ngases=ngases)

  return(col)
})

### Method gcol
setMethod("gcol", "ANY", function(object, gases=0, pal, ...)
{
  if(sum(gases == 0)) gases <- gases(object)
  # gases: color
  # A: 1, B: 2, C: 3 
  # AB: 4, AC: 5, BC: 6
  # ABC: 7
  npal <- 7  
  if(missing(pal)) pal <- brewer.pal(npal, "Set1") 
  
  col <- pal[gases]
  
  return(col)
})
