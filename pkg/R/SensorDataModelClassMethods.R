# last revision: 10 Jan 2012

#' @include spline.lib.R
NULL

#----------------------------
# Class defintion
#----------------------------

#' Class SensorDataModel.
#' @name SensorDataModel-class
#' @rdname SensorDataModel-class
#' @exportClass SensorDataModel
{}

setClass("SensorDataModel")


#----------------------------
# Class constructor
#----------------------------

#' Constructor method of SensorDataModel Class.
#'
#' @param model Name of the data model. Supported names: 
#'   \code{"pls"}, \code{"pls"}, \code{"mvr"}, \code{"broken-stick"} and \code{"ispline"}.
#' @param ... Parameters of constructor.
#' @return An object of class \code{SensorDataModel}.
#'
#' @name SensorDataModel
#' @rdname SensorDataModel-class
#' @export
SensorDataModel <- function(model, ...)
{
  switch(model, 
    "plsr" = initDataModelMvr(method=model, ...),
    "mvr" = initDataModelMvr(method=model, ...),
    "broken-stick" = initDataModelLm(method=model, ...),    
    "ispline" = initDataModelSpline(method=model, ...),
    stop("Error in SensorDataModel: 'model' is unknown."))
}


### Function 'initDataModelMvr'
initDataModelMvr <- function(method, X, C, ncomp=ncol(C), 
  centerX=TRUE, centerY=TRUE, scaleX=FALSE, scaleY=FALSE, 
  coeffNonneg = FALSE, coeffNonnegTransform = "zero", ...)
{ 
  out <- list(ncomp=ncomp, method=method, type = 'mvr',
	  centerX=centerX, centerY=centerY, scaleX=scaleX, scaleY=scaleY)
  
  # convert to matrix
  Y <- as.matrix(X)
  X <- as.matrix(C)  

  if(is.null(nrow(X))) X <- matrix(X, nrow = 1)
  if(is.null(nrow(Y))) Y <- matrix(Y, nrow = 1)  
   
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
  
  if(method == 'mvr') {
    if(coeffNonneg) {
      # nonneg. constraints
      Amat <- diag(1, ncol(X))  
  
      # regression
	    XTX <- t(X) %*% X

      dvec <- as.vector(t(X) %*% Y)    
	    Dmat <- XTX
  
      qp.fit <- solve.QP(Dmat=Dmat, dvec=dvec, Amat=Amat, meq=0, factorized=FALSE)
      coefficients <- qp.fit$solution
  
      tol <- 1e-10
      ind <- which(coefficients < 0 & abs(coefficients) < tol)
      coefficients[ind] <- 0
      if(sum(coefficients < 0))
        stop("Error in 'SensorDataModel::initDataModelMvr': some coeff. are negative after quad. prog.")    
  
	    out <- c(out, list(coefficients=coefficients))
	  }
	  else {
	    mod <- mvr(Y ~ X, ncomp=ncomp, scale=FALSE )
    	out <- c(out, list(coefficients=mod$coefficients[, , ncomp]))	    
	  }
	}
	else if(method == 'plsr') {
    mod <- plsr(Y ~ X, ncomp=ncomp, scale=FALSE)
  	out <- c(out, list(coefficients=mod$coefficients[, , ncomp]))
	}

	oldClass(out) <- "SensorDataModel"

  return(	out)
}

### Function 'initDataModelLm'
initDataModelLm <- function(method, X, C, ncomp=ncol(C),
  coeffNonneg = FALSE, coeffNonnegTransform = "zero", ...)
{ 
  out <- list(ncomp=ncomp, method=method, type = 'lm')
  
  # convert to matrix
  Y <- as.matrix(X)
  X <- as.matrix(C)  
  
  if(is.null(nrow(X))) X <- matrix(X, nrow = 1)
  if(is.null(nrow(Y))) Y <- matrix(Y, nrow = 1)  
    
  nc <- ncol(X)
     
  if(method == 'broken-stick') {
    if(ncol(Y) != 1)
      stop("Error in 'initDataModelLm': 'ncol(Y) != 1'.")    
    p <- 1
    p0 <- 2
    
    Xc <- apply(X, 2, function(x) min(x[x != 0])) # min of non-zero values
    Xc <- Xc /2 
    coefficients <- rep(NA, p * nc)
    coefficients0 <- rep(NA, p0 * nc)    
    names(coefficients) <- rep("rx", nc)
    for(i in 1:nc) {
      x <- X[, i]
      y <- as.numeric(Y)
      
      ind <- x != 0
      x <- x[ind]
      y <- y[ind]
      
      c <- Xc[i]
      dat <- data.frame(y=y, lx=lx(x, c), rx=rx(x, c))
      
      mod <- lm(y ~ rx, dat) 
      coefi <- mod$coefficients

      # complete coef. with 'lx' coefficient
      coefi2 <- - coefi[1] / c
      coefi <- c(coefi[1], coefi2, coefi[2])
      
      # check 'rx' coefficient (non-negative)
      if(coeffNonneg) {
        if(coefi[3] < 0) {
          coefi[3] <- switch(coeffNonnegTransform,
            "zero" = 0,
            stop("Error in 'initDataModelLm': 'coeffNonnegTransform' is unknown."))
        }
      }
      
      ii <- 1:p0 + p0*(i-1)
      coefficients0[ii] <- coefi[1:p0]
      ii <- 1:p + p*(i-1)
      coefficients[ii] <- coefi[p0+1]
  	}
	  out <- c(out, list(coefficients=coefficients, coefficients0=coefficients0, Xc=Xc, p=p, p0=p0))  	
	}
	else     
	  stop("Error in 'initDataModelLm': 'method' (", method, "), is unknown.")

	oldClass(out) <- "SensorDataModel"

  return(	out)
}

### Function 'initDataModelSpline'
initDataModelSpline <- function(method, X, C, ncomp=ncol(C), ...)
{ 
  out <- list(ncomp = ncomp, method = method, type = 'spline')
  
  # convert to matrix
  Y <- as.matrix(X)
  X <- as.matrix(C)  
  
  if(is.null(nrow(X))) X <- matrix(X, nrow = 1)
  if(is.null(nrow(Y))) Y <- matrix(Y, nrow = 1)  
    
  nc <- ncol(X)
     
  if(method == 'ispline') {
    if(ncol(Y) != 1)
      stop("Error in 'initDataModelSpline': 'ncol(Y) != 1'.")    
    p <- 3
    p0 <- 1
    
    coefficients <- rep(NA, p * nc)
    coefficients0 <- rep(NA, p0 * nc)
    transform <- list()
    names(coefficients) <- rep(paste("ispline", 1:p, sep="-"), nc)
    for(i in 1:nc) {
      x <- X[, i]
      y <- as.numeric(Y)
      
      ind <- x != 0
      x <- x[ind]
      y <- y[ind]
      
      breaks <- c(0, min(x)/2, 1.1 * max(x), 1.2 * max(x))
      bs <- SplineBasis("iqspline", norder=2, breaks = breaks, last=1)
      beta <- fitSplineBasis(bs, x, y)

      ii <- 1:p + p*(i-1)
      coefficients[ii] <- beta[1:p]
      ii <- 1:p0 + p0*(i-1)
      coefficients0[ii] <- beta[p+1]

      transform[[i]] <- bs
  	}
	  out <- c(out, list(transform=transform, coefficients=coefficients, coefficients0=coefficients0, p=p, p0=p0))  	
	}
	else     
	  stop("Error in 'initDataModelSpline': 'method' (", method, "), is unknown.")

	oldClass(out) <- "SensorDataModel"

  return(	out)
}


### Function 'initDataModelMvrBrokenStick'
initDataModelMvrBrokenStick <- function(X, C, ncomp=ncol(C), 
  centerX=TRUE, centerY=TRUE, scaleX=FALSE, scaleY=FALSE)
{  
  out <- list(ncomp=ncomp, method="mvr-broken-stick",
	  centerX=centerX, centerY=centerY, scaleX=scaleX, scaleY=scaleY)
  
  # convert to matrix
  Y <- as.matrix(X)
  X <- as.matrix(C)  
  
  Xc <- apply(X, 2, function(x) min(x[x != 0])) # min of non-zero values
  Xc <- Xc /2 
 
  rX <- rx(X, Xc)
  X <- rX 
  
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
  
  # left-side X: variables 'Yc'
  Xc.mat <- matrix(Xc, nrow=1)
  rXc <- matrix(rx(Xc.mat, Xc), nrow=1)
  if(out$centerX[1]) rXc <- as.matrix(sweep(rXc, 2, out$centerX, "-"))
  if(out$scaleX[1]) rXc <- as.matrix(sweep(rXc, 2, out$scaleX, "/"))
  Yc <- as.numeric(rXc %*% coefficients) # right-side model response on the boundary 'Xc'
  
  coef0 <- -out$centerY - Yc
  coef2 <- - (Yc - coef0) / as.numeric(Xc)
  coef0 <- coef0 + ncol(X) * (Yc - coef0)
  
	out <- c(out, list(Xc=Xc, Yc=Yc, coefficients=coefficients, coef0=coef0, coef2=coef2))

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
  cat(" - method: ", x$method, " (type: ", x$type, ")\n", sep='')
  if(x$method == 'plsr') 
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

plot.DataModel.pca <- function(x, y, C, Y, mod, ...)
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
  X <- switch(object$type,
    'mvr' = predict.SensorDataModel.mvr(object, C, B, ...),
    'lm' = predict.SensorDataModel.lm(object, C, B, ...),
    'spline' = predict.SensorDataModel.spline(object, C, B, ...),    
    stop("Error in 'predict.SensorDataModel': slot 'type' (", object$type, "), is unknown."))
  
  return(X)
}

predict.SensorDataModel.mvr <- function(object, C, B, out = "sum", ...)  
{
  if(missing(B)) B <- coefficients(object)
  if(is.null(nrow(B))) B <- matrix(B, nrow=1)
  if(is.null(nrow(C))) C <- matrix(C, nrow=1)
  
  # check par.
  if(out != 'sum')
    stop("Error in 'predict.SensorDataModel.mvr': parameter 'out' is not 'sum'.")

  C <- as.matrix(C)
  n <- nrow(C)
  ng <- apply(C, 1, function(x) sum(x != 0))
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
  else
    stop("Error in 'predict.SensorDataModel.mvr': 'n' ", n, ", 'nb'", nb, ".")
    
  # un-scaling
  if(object$scaleY[1]) X <- as.matrix(sweep(X, 2, object$scaleY, "*"))  
  if(object$centerY[1]) X <- as.matrix(sweep(X, 2, object$centerY, "+")) 
  # NOTE: centering by factor 'ng'  
  #centerY.factor <- rep(1, n)
  #ind <- (ng > 1)
  #centerY.factor[ind] <- centerY.factor[ind] + 0.1 * ng[ind]
  #if(object$centerY[1]) X <- apply(X, 2, function(x) x + centerY.factor * object$centerY) 
    
  return(X)
}

predict.SensorDataModel.lm <- function(object, C, B, out = "sum", ...)  
{
  if(missing(B)) B <- coefficients(object)
  if(is.null(nrow(B))) B <- matrix(B, nrow=1)
  if(is.null(nrow(C))) C <- matrix(C, nrow=1)

  C <- as.matrix(C)
  B0 <- object$coefficients0
  nc <- ncol(C)
  p <- object$p
  p0 <- object$p0  
  n <- nrow(C)
  nb <- nrow(B)
  
  # predict
  X <- matrix(NA, nrow=n, ncol=nc)
  if(nb == 1) {
    if(object$method == 'broken-stick') {    
      for(k in 1:nc) {
        kk <- 1:p + p*(k-1)
        Bk <- B[kk]
        kk <- 1:p0 + p0*(k-1)
        Bk <- c(B0[kk], Bk)
        
        Ck <- C[, k]
        c <- object$Xc[k]

        X[, k] <- Bk[1] + Bk[2] * lx(Ck, c) + Bk[3] * rx(Ck, c)
      }      
    }
    else
      stop("Error in 'predict.SensorDataModel.lm': 'method'", object$method, "is unkonwn.")    
  }
  else if(nb == n) {
    X <- matrix(NA, nrow=n, ncol=nc)
    for(i in 1:nb) {
      Bi <- B[i, ]
      Ci <- C[i, ]
      if(object$method == 'broken-stick') {    
        for(k in 1:nc) {
          kk <- 1:p + p*(k-1)
          Bk <- Bi[kk]
          kk <- 1:p0 + p0*(k-1)
          Bk <- c(B0[kk], Bk)
          c <- object$Xc[k]
          
          X[i, k] <- Bk[1] + Bk[2] * lx(Ci[k], c) + Bk[3] * rx(Ci[k], c)
        }
      }
      else
        stop("Error in 'predict.SensorDataModel.lm': 'method'", object$method, "is unkonwn.")           
    }
  }
  else
    stop("Error in 'predict.SensorDataModel.lm': 'n' ", n, ", 'nb'", nb, ".")

  # sum 'X' along columns
  if(out == 'sum') {
    X <- apply(X, 1, sum)
    X <- matrix(X, ncol=1)
  }
  else if(out == 'gas') {
    if(ncol(X) != nc)
      stop("Error in 'predict.SensorDataModel.lm': 'out' is set to 'gas', but #columns of 'X' is incorrect.")
  }
  else
    stop("Error in 'predict.SensorDataModel.lm': 'out' is unknown.")
  
  return(X)
}

predict.SensorDataModel.spline <- function(object, C, B, out = 'sum', ...)  
{
  if(missing(B)) B <- coefficients(object)
  if(is.null(nrow(B))) B <- matrix(B, nrow=1)
  if(is.null(nrow(C))) C <- matrix(C, nrow=1)

  C <- as.matrix(C)
  B0 <- object$coefficients0
  p0 <- object$p0  
  nc <- ncol(C)
  p <- object$p
  n <- nrow(C)
  ng <- apply(C, 1, function(x) sum(x != 0))
  nb <- nrow(B)
  
  # predict
  X <- matrix(NA, nrow=n, ncol=nc)
  if(nb == 1) {
    if(object$method == 'ispline') {
      for(k in 1:nc) {
        kk <- 1:p + p*(k-1)
        Bk <- B[kk]
        kk <- 1:p0 + p0*(k-1)
        Bk <- c(Bk, B0[kk])
        
        Ck <- C[, k]
        
        bs <- object$transform[[k]]
        X[, k] <- predict(bs, Ck, Bk)
      }      
    }
    else
      stop("Error in 'predict.SensorDataModel.spline': 'method'", object$method, "is unkonwn.")    
  }
  else if(nb == n) {
    X <- matrix(NA, nrow=n, ncol=nc)
    for(i in 1:nb) {
      Bi <- B[i, ]
      Ci <- C[i, ]
      if(object$method == 'ispline') {    
        for(k in 1:nc) {
          kk <- 1:p + p*(k-1)
          Bk <- Bi[kk]
          kk <- 1:p0 + p0*(k-1)
          Bk <- c(Bk, B0[kk])

          Ck <- Ci[k]
          
          bs <- object$transform[[k]]
          X[i, k] <- predict(bs, Ck, Bk)
        }
      }
      else
        stop("Error in 'predict.SensorDataModel.spline': 'method'", object$method, "is unkonwn.")           
    }
  }
  else
    stop("Error in 'predict.SensorDataModel.spline': 'n' ", n, ", 'nb'", nb, ".")

  # sum 'X' along columns
  if(out == 'sum') {
    X <- apply(X, 1, sum)
    X <- matrix(X, ncol=1)
  }
  else if(out == 'gas') {
    if(ncol(X) != nc)
      stop("Error in 'predict.SensorDataModel.spline': 'out' is set to 'gas', but #columns of 'X' is incorrect.")
  }
  else
    stop("Error in 'predict.SensorDataModel.spline': 'out' is unknown.")

  return(X)
}
#----------------------------
# Other Class Methods
#----------------------------

#' @S3method ncoef SensorDataModel
ncoef.SensorDataModel <- function(object)
{
  length(coef(object))
}
