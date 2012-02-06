# last revision: 23 Jan 2012

#----------------------------
# Class defintion
#----------------------------

#' Class DriftCommonModel.
#' @name DriftCommonModel-class
#' @rdname int-DriftCommonModel-class
#' @keywords class
#' @exportClass DriftCommonModel
{}

setClass("DriftCommonModel")


#----------------------------
# Class constructor
#----------------------------

#' @export
DriftCommonModel <- function(dmodel, ...)
{
  switch(dmodel, 
    "cpc" = initCommonModelCpcPlsr(...),
    stop("Error in DriftCommonModel: 'dmodel' is unknown."))
}

### Function 'initCommonModelCpcPlsr'

#' Build a common model based on common principal component analysis.
#'
#' @return A \code{\link{DriftCommonModel}} object.
#' @name initCommonModelCcPlsr
#' @rdname int-initCommonModelCcPlsr
#' @keywords DriftNoiseModel
initCommonModelCpcPlsr <- function(dspace, ndcomp, ndvar, 
  centered=TRUE, scaled=TRUE, center=centered, scale=scaled, centerX=NULL, scaleX=NULL, ...)
{   
	out <- list(method="cpc", dspace=dspace, ndcomp=ndcomp, ndvar=ndvar,
    centered=centered, scaled=scaled, center=center, scale=scale,
    centerX=centerX, scaleX=scaleX)
    
	oldClass(out) <- "DriftCommonModel"

  return(	out)
}

#----------------------------
# Print Methods
#----------------------------
#' @S3method print DriftCommonModel
print.DriftCommonModel <- function(x, ...)
{
  cat(" drift common model\n")
  cat(" - method:", x$method, "\n")
  cat(" - ndcomp:", x$ndcomp, "\n")  
}

#----------------------------
# Plot Methods
#----------------------------

#' @S3method plot DriftCommonModel
plot.DriftCommonModel <- function(x, y="prediction", ...)
{
  switch(y,
    prediction=plot.DriftCommonModel.prediction(x, y, ...),
    pca=plot.DataModel.pca(x, y, ...)) 
}

plot.DriftCommonModel.prediction <- function(x, y, C, p = 1,
  main = "Prediction: y ~ c", xlab = paste("Concentration of gas", p), ylab = "Sensor Signal",
  jitter = FALSE, ...)
{
  if(missing(C))
    stop("Error in 'plot.DriftCommonModel.prediction': 'C' is missing.")
  
  X <- predict(x, C)  

  # plot points
  ind <- (C[, 1] != 0)  
  cp <- C[ind, 1]
  if(jitter) cp <- jitter(cp)  
  xp <- X[ind, 1]
  if(jitter) xp <- jitter(xp)
  
  if(jitter) main <- paste(main, " (jittered points)", sep='')
  plot(cp, xp, bty='n',
    main=main, xlab = xlab, ylab = ylab)  
}

#----------------------------
# Predict Methods
#----------------------------

#' @S3method predict DriftCommonModel
predict.DriftCommonModel <- function(object, X, dsd, ...)
{
  if(missing(X)) stop("Error in 'predict.DriftCommonModel': missing 'X'.")
  if(missing(dsd)) stop("Error in 'predict.DriftCommonModel': missing 'dsd'.")
  
  nsensors <- nrow(object$dspace)
  if(ncol(X) != nsensors)
    stop("Error in 'predict.DriftCommonModel': 'ncol(X) != nsensors'.")
    
  if(!dsd)
    return(X)

  # drift noise 'N'
  ndcomp.str <- ifelse(object$ndcomp == 1, "one", "many")  
  varcov <- switch(ndcomp.str,
    "one" = dsd * object$ndvar * diag(1),
    dsd * diag(object$ndvar)) # ~ 86.23% 7.25% and 3.26%
  N <- rmnorm(n = nrow(X), varcov = varcov)
  N <- N %*% t(object$dspace[, 1:object$ndcomp])     
  
  # compute
  centerX <- apply(X, 2, mean)
  scaleX <- apply(X, 2, sd)
  
  # scale
  if(object$center) X <- as.matrix(sweep(X, 2, centerX, "-"))
  if(object$scale) X <- as.matrix(sweep(X, 2, scaleX, "/"))  
  
  # inject drift
  X <- X + N
  
  # un-scale
  if(object$scale) X <- as.matrix(sweep(X, 2, scaleX, "*"))
  if(object$center) X <- as.matrix(sweep(X, 2, centerX, "+"))

  return(X)
}