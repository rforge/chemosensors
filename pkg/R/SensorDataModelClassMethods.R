# last revision: 10 Jan 2012

#----------------------------
# Class defintion
#----------------------------

#' Class SensorDataModel.
#' @name SensorDataModel-class
#' @rdname int-SensorDataModel-class
#' @keywords class
#' @exportClass SensorDataModel
{}

setClass("SensorDataModel")


#----------------------------
# Class constructor
#----------------------------

#' @export
SensorDataModel <- function(model, ...)
{
  switch(model, 
    "plsr" = initDataModelPlsr(...),
    "mvr" = initDataModelMvr(...),
    stop("Error in SensorDataModel: 'model' is unknown."))
}

### Function 'initDataModelPlsr'

#' Build a PLSr-based sensor model.
#'
#' @param X Sensor signals (model response).
#' @param C Concentration matrix (model predictors).
#' @return A \code{\link{SensorDataModel}} object.
#' @name initDataModelPlsr
#' @rdname int-initDataModelPlsr
#' @keywords SensorModel
initDataModelPlsr <- function(X, C, ncomp=ncol(C), 
  centerX=TRUE, centerY=TRUE, scaleX=FALSE, scaleY=FALSE)
{   
	out <- list(ncomp=ncomp, method="plsr", 
    centerX=centerX, centerY=centerY, scaleX=scaleX, scaleY=scaleY)
  
  # convert to matrix
  C <- as.matrix(C)
  X <- as.matrix(X)
   
  # PLSr requiers scaling of predictors 'C'
  if(out$centerX) {
    centerX <- apply(C, 2, mean)
    C <- sweep(C, 2, centerX)
    out$centerX <- centerX
  }
  if(out$scaleX) {
    scaleX <- apply(C, 2, sd)
    C <- sweep(C, 2, scaleX, "/")
    out$scale <- scaleX
  }

  # PLSr requiers scaling of responce 'X'
  if(out$centerY) {
    centerY <- apply(X, 2, mean)
    X <- sweep(X, 2, centerY)
    out$centerY <- centerY
  }
  if(out$scaleY) {
    scaleY <- apply(X, 2, sd)
    X <- sweep(X, 2, scaleY, "/")
    out$scaleY <- scaleY
  }
  
	mod <- plsr(X ~ C, ncomp=ncomp, scale=FALSE)
	out <- c(out, list(coefficients=mod$coefficients[, , ncomp]))

	oldClass(out) <- "SensorDataModel"

  return(	out)
}

### Function 'initDataModelMvr'
initDataModelMvr <- function(X, C, ncomp=ncol(C), 
  centerX=TRUE, centerY=TRUE, scaleX=FALSE, scaleY=FALSE)
{   
 out <- list(ncomp=ncomp, method="mvr",
	  centerX=centerX, centerY=centerY, scaleX=scaleX, scaleY=scaleY)
  
  # convert to matrix
  Y <- as.matrix(X)
  X <- as.matrix(C)  
   
  # Scale predictors 'X'
  if(out$centerX) {
    centerX <- apply(X, 2, mean)
    X <- sweep(X, 2, centerX)
    out$centerX <- centerX
  }
  if(out$scaleX) {
    scaleX <- apply(X, 2, sd)
    X <- sweep(X, 2, scaleX, "/")
    out$scale <- scaleX
  }

  # Scale responce 'Y'
  if(out$centerY) {
    centerY <- apply(Y, 2, mean)
    Y <- sweep(Y, 2, centerY)
    out$centerY <- centerY
  }
  if(out$scaleY) {
    scaleY <- apply(Y, 2, sd)
    Y <- sweep(Y, 2, scaleY, "/")
    out$scaleY <- scaleY
  }
  
  # nonneg. constraints
  Amat <- diag(1, ncol(X))  
  
  # regression
	XTX <- t(X) %*% X

  dvec <- as.vector(t(X) %*% Y)    
	Dmat <- XTX
  
  qp.fit <- solve.QP(Dmat=Dmat, dvec=dvec, Amat=Amat, meq=0, factorized=FALSE)
  coefficients <- qp.fit$solution
  
	out <- c(out, list(coefficients=coefficients))

	oldClass(out) <- "SensorDataModel"

  return(	out)
}

#----------------------------
# Print Methods
#----------------------------
#' @S3method print SensorDataModel
print.SensorDataModel <- function(x, ...)
{
  cat(" data model\n")
  cat(" - method:", x$method, "\n")
  cat(" - ncomp:", x$ncomp, "\n")  
}

#----------------------------
# Plot Methods
#----------------------------

#' @S3method plot SensorDataModel
plot.SensorDataModel <- function(x, y="prediction", ...)
{
  switch(y,
    prediction=plot.SensorDataModel.prediction(x, y, ...),
    pca=plot.DataModel.pca(x, y, ...)) 
}

plot.SensorDataModel.prediction <- function(x, y, C, p = 1,
  main = "Prediction: y ~ c", xlab = paste("Concentration of gas", p), ylab = "Sensor Signal",
  jitter = FALSE, ...)
{
  if(missing(C))
    stop("Error in 'plot.SensorDataModel.prediction': 'C' is missing.")
  
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

plot.SensorDataModel.pca <- function(x, y, C, Y, ...)
{
  if(missing(C) | missing(Y))
    stop("Error in 'plot.SensorDataModel.prediction': 'C' or 'Y' is missing.")

  X <- predict(mod, C)
  scoreplot(prcomp(X), col=as.numeric(Y), ...)
}

#----------------------------
# Predict Methods
#----------------------------

# @S3method ncoef SensorDataModel
#ncoef.SensorDataModel <- function(x)
#{
#  length(coef(x))
#}

#' @S3method predict SensorDataModel
predict.SensorDataModel <- function(object, C, B, ...)
{
  if(missing(B)) B <- coefficients(object)
  if(is.null(nrow(B))) B <- matrix(B, ncol=length(B))

  C <- as.matrix(C)
  n <- nrow(C)
  nb <- nrow(B)

  # scaling
  if(object$centerX[1]) C <- as.matrix(sweep(C, 2, object$centerX, "-"))
  if(object$scaleX[1]) C <- as.matrix(sweep(C, 2, object$scaleX, "/"))
  
  # predict
  if(nb == 1) {
    X <- C %*% t(B)
  }
  else if(nb == n) {
    X <- matrix(NA, nrow=n, ncol=1)
    for(i in 1:nb) {
      X[i, ] <- C[i, ] %*% B[i, ]
    }
  }
    
  # un-scaling
  if(object$scaleY[1]) X <- as.matrix(sweep(X, 2, object$scaleY, "*"))
  if(object$centerY[1]) X <- as.matrix(sweep(X, 2, object$centerY, "+"))

  return(X)
}
