# last revision: 10 Jan 2012

#' @include SensorDynamicsClass.R
NULL

#----------------------------
# Class constructor
#----------------------------

#' Get default constructor parameters of class \code{\link{SensorDynamics}}.
#' @name defaultParSensorDynamics
#' @rdname pub-defaultParSensorDynamics
#' @keywords SensorDynamics defaults
#' @return List of the default parameters.
#@example inst/examples/defaultParSensorDynamics.R
#' @export
defaultParSensorDynamics <- function()
{
  ntconst <- 2
  trange <- array(c(0.50, 0.80, 0.01, 0.05), c(2, ntconst))
  
  par <- list(num=1, gases=1:3, gnames=LETTERS[1:3], concUnits=defaultConcUnits(), concUnitsInt=defaultConcUnitsInt(),
    tunit = 60, ntconst=ntconst, trange=trange)
  
  return(par)
}

### Constructor of SensorDynamics class.
setMethod("initialize", "SensorDynamics", function(.Object,
  # common for sub-classes
  num="numeric", gases="numeric", gnames="character", concUnits="character", concUnitsInt="character",
  # class-specific 
  tunit = "numeric", ntconst = "numeric", trange = "array", ...)
{   
  # missing
  def.par <- defaultParSensorDynamics()
  
  if(missing(num)) num <- def.par$num
  nsensors <- length(num)
    
  if(missing(gases)) gases <- def.par$gases
  if(missing(gnames)) gnames <- def.par$gnames
  if(missing(concUnits)) concUnits <- def.par$concUnits
  if(missing(concUnitsInt)) concUnitsInt <- def.par$concUnitsInt

  if(missing(tunit)) tunit <- def.par$tunit
  enableDyn <- (tunit > 1)

  if(missing(ntconst)) ntconst <- def.par$ntconst  
  if(missing(trange)) trange <- def.par$trange

  # filter by 'gases'  
  gnames <- gnames[gases]
  ngases <- length(gases)
  gind <- rep(NA, max(gases))
  for(i in 1:length(gases)) {
    g <- gases[i]
    gind[g] <- i
  }
  
  # check 'trange'
  if(length(dim(trange)) == 2)
    trange <- array(trange, c(dim(trange), nsensors))
  dimnames(trange) <- list(c("min", "max"), paste("tconst", 1:ntconst, sep=''), paste("S", 1:nsensors, sep=''))
  
  # assign
  .Object@num <- num
  .Object@gases <- gases
  .Object@gind <- gind
  .Object@ngases <- ngases
  .Object@gnames <- gnames
  .Object@concUnits <- concUnits
  .Object@concUnitsInt <- concUnitsInt      

  .Object@tunit <- tunit
  .Object@enableDyn <- enableDyn
  .Object@ntconst <- ntconst          
  .Object@trange <- trange   

  # set up 'tconst' 
  tconst <- array(0, c(ngases, ntconst, nsensors))   
  dimnames(tconst) <- list(gnames, paste("tconst", 1:ntconst, sep=''), paste("S", 1:nsensors, sep=''))
  for(s in 1:nsensors) {
    for(i in 1:.Object@ntconst) {
      tconst[, i, s] <- runif(n=ngases, min=.Object@trange["min", i, s], max=.Object@trange["max", i, s]) 
    }
  }
  
  .Object@tconst <- tconst           
          
  validObject(.Object)
  return(.Object)
})

#' @export
SensorDynamics <- function(...)
{
  new("SensorDynamics", ...)
}

#----------------------------
# Plot Methods
#----------------------------
setMethod("plot", "SensorDynamics", function (x, y, ...) 
{
  yval <- c("predict")

  # missing
  if(missing(y)) y <- "predict"
  
  switch(y,
    predict = plot.SensorDynamics.predict(x, y, ...),
    stop("Error in SensorDynamics::plot: plot type 'y' is unknown."))
})

plot.SensorDynamics.predict <- function(x, y, 
  conc, sdata, sind,
  n = 1, concUnits = "default",
  col, lty = c(3, 1), lwd = 2,
  main = "Sensor Dynamics", xlab = "Samples", ylab="Sensor Signal", ...)
{
  if(concUnits == "default") concUnits <- concUnits(x)

  nsensors <- nsensors(x)
  ngases <- ngases(x)

  if(missing(conc)) conc <- concSampleDyn(x, n=n, concUnits=concUnits, ...)
  if(missing(sdata)) sdata <- array(conc, c(dim(conc), nsensors))
  if(missing(sind)) sind <- 1
      
  if(missing(col)) col <- gcol(x)
  if(missing(main)) main <- paste(main, ": tunit ", tunit(x), ", sensor ", sind, sep="")  

  nsdata <- predict(x, conc, sdata, sensors=sind, ...)  

  lty <- rep(lty, each=ngases)
  yp <- sdata[, , sind]
  nyp <- nsdata
  
  matplot(cbind(yp, nyp), t='l', col=col, lwd = lwd, lty = lty,
    bty='n',
    main=main, xlab = xlab, ylab = ylab)  
}


#----------------------------
# Predict Methods
#----------------------------

### Method coefficients
setMethod("coefficients", "SensorDynamics", function(object, ...)
{
  coef <- tconst(object)
  
  return(coef)
})

### Method ncoef
setMethod("ncoef", "SensorDynamics", function(x)
{
  length(coef(x))
})

### Method predict
setMethod("predict", "SensorDynamics", function(object, conc, sdata, sensors, ...)
{
  if(missing(conc)) stop("Error in SensorDynamics::predict: 'conc' is missing.")
  if(missing(sdata)) stop("Error in SensorDynamics::predict: 'sdata' is missing.")  
  if(missing(sensors)) sensors <- 1
  
  nsensors <- length(sensors)
  ngases <- ngases(object)
  n <- nrow(conc)
  tconst <- tconst(object)
  
  if(nsensors == 1) { # case of single sensor
    if(length(dim(sdata)) == 2) sdata <- array(sdata, c(dim(sdata), 1)) 
    else if(length(dim(sdata)) == 3) sdata <- array(sdata[, , sensors], c(dim(sdata)[1:2], 1)) 
  }
    
  if(length(dim(conc)) != 2) stop("Error in SensorDynamics::predict: 'conc' is not 2D.")
  if(length(dim(sdata)) != 3) stop("Error in SensorDynamics::predict: 'sdata' is not 3D.")          
  
  if(dim(sdata)[3] != nsensors) stop("Error in SensorDynamics::predict: 'dim(sdata)[3] != nsensors'.")    
  if(sum(dim(sdata)[-3] == dim(conc)) != 2) stop("Error in SensorDynamics::predict: 'conc' and 'sdata' are not compatible in 1-2 dimensions.")    
  
  nsdata <- array(0, dim(sdata)) # new 'sdata' 
  for(si in 1:nsensors) {
    s <- sensors[si]
    for(i in 1:ngases) {
      tconsti <- tconst[i, , s]
      sdatai <- as.numeric(sdata[, i, si])
      # filter
      nsdatai <- filter(x=sdatai, filter=c(0, tconsti), method="recursive")
      # normalize
      nsdatai.max <- max(nsdatai)
      if(nsdatai.max)  nsdatai <- nsdatai * (max(sdatai) / nsdatai.max)
      
      nsdata[, i, si] <- nsdatai
    }
  }
  
  if(nsensors == 1) nsdata <- nsdata[, , 1]

  return(nsdata)
})

#----------------------------
# Model Methods
#----------------------------

