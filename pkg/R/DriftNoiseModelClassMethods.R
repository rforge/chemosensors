# last revision: 23 Jan 2012

#' @include DriftNoiseModelClass.R
NULL

#----------------------------
# Class constructor
#----------------------------

#' Get model names of class \code{\link{DriftNoiseModel}}.
#' @name DriftModelNames
#' @rdname pub-DriftModelNames
#' @keywords DriftNoiseModel
#' @return Character vector of model names.
#@example R/example/getDriftModelNames.R
#' @export
DriftModelNames <- function()
{
  return(c("cpc"))
}

#' Get default constructor parameters of class \code{\link{DriftNoiseModel}}.
#' @name defaultParDriftNoiseModel
#' @rdname pub-defaultParDriftNoiseModel
#' @keywords DriftNoiseModel defaults
#' @return List of the default parameters.
#@example R/example/defaultParDriftNoiseModel.R
#' @export
defaultParDriftNoiseModel <- function()
{
  par <- list(num=1:2, 
    dsd = 0.1, ndcomp = 2, ndvar = NULL, dmodel = "cpc",
    datasetDriftNoiseModel=defaultDataDriftNoiseModel(), pck=defaultDataPackage())

  return(par)
}

### Constructor of DriftNoiseModel class.
setMethod("initialize", "DriftNoiseModel", function(.Object,
  # common for sub-classes
  num = "numeric", 
  # specific for class DriftNoiseModel
  datasetDriftNoiseModel = "character", pck = "character", 
  dsd = "numeric", ndcomp = "numeric", ndvar,  
  dmodel = "character", ...)
{   
  # missing
  def.par <- defaultParDriftNoiseModel()
  
  if(missing(num)) num <- def.par$num
  if(missing(datasetDriftNoiseModel)) datasetDriftNoiseModel <- def.par$datasetDriftNoiseModel
  if(missing(pck)) pck <- def.par$pck
  if(missing(dsd)) dsd <- def.par$dsd  
  if(missing(ndcomp)) ndcomp <- def.par$ndcomp  
  if(missing(ndvar)) ndvar <- def.par$ndvar      
  if(missing(dmodel)) dmodel <- def.par$dmodel
  
  # check parameters
  if(length(num) < ndcomp)
    stop("Error in DriftNoiseModel::initialize: 'ndcomp' is incosistent with 'num'.")  
  
  # check 'dmodel'
  is.correct.dmodel <- (dmodel %in% DriftModelNames())
  if(!is.correct.dmodel)
    stop("Error in DriftNoiseModel::initialize: name of 'dmodel' is incorrect.")  
     
  # load data
  data(list=datasetDriftNoiseModel, package=pck, envir=environment()) # -> 'centered', 'scaled', 'dspace', 'ndvar'
  if(!exists("dspace"))
    stop("Error in DriftNoiseModel::initialize: 'datasetDriftNoiseModel' is not loaded; variable 'dspace' not found.")    
  
  # set 'ndvar'
  ndvar <- ndvar[1:ndcomp]
  
  # check 'num'
  if(sum(num <= 0 | num > ncol(dspace)))
    stop("Error in DriftNoiseModel::initialize: 'num' is incorrect.")  
    
  # modify, normalize and orthogonolize 'dspace'
  dspace <- dspace[num, num] 
  if(length(num) == 1) dspace <- matrix(dspace, nrow=1, ncol=1)
  dspace.norm <- apply(dspace, 2, function(x) sqrt(sum(x*x)))
  dspace <- sweep(dspace, 2, dspace.norm, "/")
  
  dspace.qr <- qr(dspace) # QR decomposition
  dspace <- qr.Q(dspace.qr)

  rownames(dspace) <- paste("num", num)
  colnames(dspace) <- paste("pc", 1:ncol(dspace))  
  
  # assign
  .Object@num <- num  
  .Object@dsd <- dsd  
  .Object@ndcomp <- ndcomp  
  .Object@ndvar <- ndvar  
      
  # assign 'dmodel'  
  .Object@driftModel <- DriftCommonModel(dmodel=dmodel, 
    dspace=dspace, ndcomp=ndcomp, ndvar=ndvar, 
    centered=centered, scaled=scaled, center=centered, scale=scaled, ...)
  
  validObject(.Object)
  return(.Object)
})

#' @export
DriftNoiseModel <- function(...)
{
  new("DriftNoiseModel", ...)
}

#----------------------------
# Plot Methods
#----------------------------
setMethod("plot", "DriftNoiseModel", function (x, y, ...) 
{
  yval <- c("noise", "pc")

  # missing
  if(missing(y)) y <- "noise"
  
  switch(y,
    pc = plot.DriftNoiseModel.pc(x, y, ...),
    noise = plot.DriftNoiseModel.noise(x, y, ...),
    stop("Error in DriftNoiseModel::plot: plot type 'y' is unknown."))
})

plot.DriftNoiseModel.noise <- function(x, y, n = 100, sdata, 
  lty = c(3, 1), lwd = 2,
  main = paste("Drift Noise: noise '", type(x), "', dsd ", dsd(x), sep=''), 
  xlab = "Samples", ylab="Sensor Signals", ...)
{
  if(missing(sdata)) sdata <- sdataSample(x, n=n)
  
  nsensors <- nsensors(x)
  
  nsdata <- predict(x, sdata=sdata, n=n, ...) # noise + sdata
   
  col <- grey.colors(nsensors, start=0.3, end=0.7) 
  
  lty <- rep(lty, each=nsensors)
  matplot(cbind(sdata, nsdata), t='l', col=col, lwd = lwd, lty = lty,
    bty='n',
    main=main, xlab = xlab, ylab = ylab, ...)  
}

plot.DriftNoiseModel.pc <- function(x, y, X,
  comp = 1:2, 
  pch = 20, col,
  main = paste("Drift:", nsensors(x), "sensors,", ndcomp(x), "components."), xlab = paste("PC1"), ylab="PC2", ...)
{
  if(missing(X))
    stop("Error in 'plot.DriftNoiseModel.pc': 'X' is missing.")

  if(nsensors(x) < 2)
    stop("Error in 'plot.DriftNoiseModel.pc': 'nsensors(x) < 2'.")

  n <- nrow(X)
  if(missing(col)) col <- rep("black", n)

  # center/scale
  center <- x@driftModel$center
  scale <- x@driftModel$scale
  if(center) {
    Xcenter <- apply(X, 2, mean)
    X <- sweep(X, 2, Xcenter)
  }
  if(scale) {
    Xscale <- apply(X, 2, sd)
    X <- sweep(X, 2, Xscale, "/")
  }

  # PCA  
  mod <- prcomp(X, center=FALSE, scale=FALSE)  
  scores <- X %*% mod$rotation[, comp]

  V <- t(dspace(x))
  Vscores <- V %*% mod$rotation[, comp]
  Vb <- apply(Vscores, 1, function(x) x[2]/x[1])
  Vvar <- apply(V, 1, function(x) capturedVarDir(X, x))
  
  ### plot
  xvar <- 100*capturedVar(X, 1, mod)
  yvar <- 100*capturedVar(X, 2, mod)    
  xlab <- paste("PC1 (", round(xvar, 0), "%)", sep='')
  ylab <- paste("PC2 (", round(yvar, 0), "%)", sep='')
  
  plot(scores, pch=pch, col=col,
    main=main, xlab=xlab, ylab=ylab, ...)

  ab.col <- grey.colors(length(Vb), start = 0.2, end = 0.8)
  for(i in 1:length(Vb)) {
    abline(a=0, b=Vb[i], col=ab.col[i])    
    coord <- Vscores[i, comp]
    coord <- (1.5 / sqrt(sum(coord^2))) * coord # scale vector to length 2
    text(coord[1], coord[2], labels=paste("CPC", i, " (", round(100*Vvar[i], 0), "%)", sep=''), col=ab.col[i])
  }
}


#----------------------------
# Predict Methods
#----------------------------

### Method predict
setMethod("predict", "DriftNoiseModel", function(object, sdata, ...)
{
  sdataModel(object, sdata=sdata, ...)  # sdataModel
})

#----------------------------
# Model Methods
#----------------------------

### Method sdataModel
setMethod("sdataModel", "DriftNoiseModel", function(object, sdata, ...)
{  
  predict(object@driftModel, X=sdata, dsd=dsd(object), ...)  
})