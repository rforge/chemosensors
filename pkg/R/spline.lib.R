#----------------------------
# Class defintion/constructor
#----------------------------

#' SplineBasis class.
#'
#' Slots: type, rangeval, nbasis, params, breaks, norder, knots. 
# type       : chr "bspline"
# rangeval   : num [1:2] 0 1
# nbasis     : num 6
# params     : num [1:3] 0.1 0.2 0.4
# breaks     : num [1:5] 0 0.1 0.2 0.4 1
# norder     : num 3
# knots      : num [1:9] 0.0 0.0 0.0 0.1 0.2 0.4 1.0 1.0 1.0
#' 
#' The number of spline basis functions: \eqn{nbasis = norder + nknots - 2}.
#'
#' @name SplineBasis-class
#' @rdname SplineBasis-class
#' @exportClass SplineBasis
{}

setClass("SplineBasis")

#' Constructor of SplineBasis class.
#'
#' @param type Type of spline basis: 'bspline', 'mspline' or 'ispline'. The default value is 'bspline'.
#' @param x Numeric vector of data points.
#' @param breaks Break points. Include the boundary knots; ordered.
#' @param rangeval The range of data points (minimum and maximum values). Must include the break points.
#' @param norder The order of the basis functions. The cubic spline basis has the order 4. The default value: 3.
#' @param lambda Smoothing parameter for quadratic solver when performing fitting with restriction conditions.
#' @param tol Tolerance value for stopping criteria in iterative procedures.
#' @param first The number of first basis functions to be omitted. Supported values: 0 or 1. The default value is 0. 
#' @param last The number of last basis functions to be omitted. Supported values: 0 or 1. The default value is 0.
#' @return An object of class 'SplineBasis'.
#'
#' @name splineBasis
#' @rdname SplineBasis-class
#' @export
splineBasis <- function(type="bspline", x, breaks, rangeval, norder=3, 
  lambda = 0.1, tol = 1e-16,
  first=0, last=0)
{
  # process 'x'
  if(!missing(x)) {
    if(missing(breaks)) {
      rangeval <- range(x)
      breaks <- c(rangeval[1], median(x), rangeval[2])
    }
    else {
      rangeval <- range(breaks)
    }
  }
  else {
    if(missing(breaks)) stop("Error in 'splineBasis': missing both 'x' and 'breaks'.")

    rangeval <- range(breaks)  
  }
  
  # process 'breaks'
  if(length(rangeval) != 2) stop("Error in 'splineBasis': 'length(rangeval) != 2'.")
  if(min(breaks) < rangeval[1]) stop("Error in 'splineBasis': breaks are out of the range.")
  if(max(breaks) > rangeval[2]) stop("Error in 'splineBasis': breaks are out of the range.")
  
  breaks <- sort(unique(c(breaks, rangeval)))
  
  # bs  
  bs <- switch(type,
    bspline=createBSplineBasis(breaks, norder),
    mspline=createMSplineBasis(breaks, norder),
    ispline=createISplineBasis(breaks, norder),
    iqspline=createIQSplineBasis(breaks, norder),    
    stop("Error in 'sbasis' function: the type", type,  "is unknown.\n"))

  # 'nknots'
  bs$nknots <- length(bs$breaks) 
  
  # Internal basis functions 'ibasis' and 'ncoef'
  ind <- 1:bs$nbasis
  if(first == 1) ind <- ind[-1]
  else if(first > 1) stop("Error in 'splineBasis' function: case 'first > 1' is not supported.\n")
  
  if(last == 1) ind <- ind[-length(ind)]
  else if(last > 1) stop("Error in 'splineBasis' function: case 'last > 1' is not supported.\n")

  bs$ncoef <- length(ind)
  bs$ibasis <- (1:bs$nbasis)[ind]

  # other slots
  bs$lambda <- lambda
  bs$tol <- tol

  # assign 'class' attribute
  oldClass(bs) <- "SplineBasis"
  
  return(bs)
}

#' Wrapper to constructor of SplineBasis Class.
#'
#' @param ... Parameters of constructor.
#' 
#' @name SplineBasis
#' @rdname SplineBasis-class
#' @export
SplineBasis <- function(...) splineBasis(...)

createBSplineBasis <- function(breaks, norder)
{
  stop("Error in `spline.lib.R`: function `createBSplineBasis` is depreciated.")
  #bs <- create.bspline.basis(breaks=breaks, norder=norder)
  bs$breaks <- breaks
  bs$norder <- norder
  #bs$knots <- knots.basisfd(bs, interior=FALSE)
 
  return(bs)
}

# Notes:
# - '2*k' additional number of knots are added 
#   (in the case of B-splines, '2*(k-1)' additional knots are used)
createMSplineBasis <- function(breaks, norder)
{
  rangeval <- range(breaks)
  deg <- norder - 1
  knots <- c(rep(rangeval[1], deg), breaks, rep(rangeval[2],deg))
  
  bs <- list(type="mspline", rangeval=rangeval, nbasis = norder + length(breaks) - 2,
    breaks=breaks, norder=norder, knots=knots)
  
  return(bs)
}

createISplineBasis <- function(breaks, norder)
{
  rangeval <- range(breaks)
  deg <- norder - 1
  knots <- c(rep(rangeval[1],deg), breaks, rep(rangeval[2], deg))
  
  bs <- list(type="ispline", rangeval=rangeval, nbasis=norder + length(breaks) - 2,
    breaks=breaks, norder=norder, knots=knots)
  
  return(bs)
}

createIQSplineBasis <- function(breaks, norder)
{
  if(norder != 2)
    stop("Error in 'createIQSplineBasis': 'norder != 2'.")
    
  rangeval <- range(breaks)
  deg <- norder - 1
  knots <- c(rep(rangeval[1], deg), breaks, rep(rangeval[2], deg))
  
  bs <- list(type="iqspline", rangeval=rangeval, nbasis=norder + length(breaks) - 2,
    breaks=breaks, norder=norder, knots=knots)
  
  return(bs)
}

#----------------------------
# Print/Show Methods
#----------------------------

#' @S3method print SplineBasis
print.SplineBasis <- function(x, ...)
{
  cat(" SplineBasis\n")
  cat(" - breaks:", paste(x$breaks, collapse=", "), "\n")
  cat(" - type:", x$type, "\n")
  cat(" - norder:", x$norder, "\n")  
  cat(" - nbasis:", x$nbasis, ifelse(x$nbasis == x$ncoef, "", paste("(",x$nbasis - x$ncoef, " not used)", sep='')), "\n")  
}

#----------------------------
# Plot Methods
#----------------------------
#' @S3method plot SplineBasis
plot.SplineBasis <- function(x, y="default", ...)
{
  plotSplineBasis(x, y, ...)
}

plotSplineBasis <- function(x, y='default',   
  bty="n",
  lwd=1, lty=1, legend.cex=1.0, add=FALSE, 
  xlab="x points", ylab="spline basis functions",
  n=100, ...)
{  
  r <- x$rangeval
  xp <- seq(r[1], r[2], length.out=n)

  M <- getSplineDesign(x, xp, all=TRUE)

  Yp <- matrix(NA, nrow=length(xp), ncol=x$nbasis)
  for(i in 1:x$nbasis) {
    coef <- rep(0, x$nbasis)
    coef[i] <- 1
    Yp[, i] <- M %*% coef
  }
  
  lty <- rep(2, x$nbasis)
  lty[x$ibasis] <- 1
  bs.lwd <- rep(lwd, x$nbasis)  
  col <- rainbow(x$nbasis)
  
  main <- paste("Basis '", x$type,"' of degree ", x$norder-1, sep="")
  
  leg <- paste("basis fun.",1:x$nbasis)
  ileg <- rep("", x$nbasis)
  ileg[-x$ibasis] <- c("(disabled)")
  leg <- paste(leg, ileg)
  
  matplot(xp, Yp, type='l', add=add,
      col=col, lty=lty, lwd=bs.lwd,
      main=main, xlab=xlab, ylab=ylab, 
      bty=bty, ...)
  abline(v=x$breaks, lwd=bs.lwd, lty=2)
 
  legend("top", legend=c(leg, paste(x$nknots, "knots")), col=c(col, 1), lty=c(lty, 2), lwd=bs.lwd, cex=legend.cex) 
}

#----------------------------
# Functions 'getSplineDesign'
#----------------------------

#' Function getSplineDesign.
#'
#' @param all Logical whther include all basis functions.
#' @return A design matrix.
#'
#' @name getSplineDesign
#' @rdname SplineBasis-class
#' @export
getSplineDesign <- function(bs, x, all = TRUE)
{
  rangeval <- bs$rangeval
  
  # process parameters
  x <- as.numeric(x)  
  
  # Check 'x' data
  x[x < rangeval[1]] <- rangeval[1]
  x[x >= rangeval[2]] <- rangeval[2] - 1e-10  
  
  # Design Matrix for B-Spline basis
  M <- switch(bs$type,
    bspline=stop("Error in `spline.lib.R`: function `getSplineDesign` does not support mode `bsplinr`"), 
      #bsplineS(x, breaks=bs$breaks, norder=bs$norder),
    mspline=getMSplineDesign(bs, x),
    ispline=getISplineDesign(bs, x),
    iqspline=getIQSplineDesign(bs, x),        
    stop("Error in 'getSplineDesign' function: the type", bs$type,  "is unknown.\n"))    
 
  if(is.null(ncol(M))) {
    M <- matrix(M, nrow=1, ncol=length(M))
  }
 
  # ajust M according to 'bs$ibasis'
  if(!all) {    
    M <- M[, bs$ibasis]
  }
 
  return(M)
}

getMSplineDesign <- function(bs, x, method="bsplines", last.inc=TRUE)
{
  stop("Error in `spline.lib.R`: function `getMSplineDesign` is depreciated.")

  # process parameters
  x <- as.numeric(x)  
  
  # Prepare data  
  n <- bs$nbasis
  k <- bs$norder 
  t <- bs$knots

  M <- matrix(0, nrow=length(x), ncol=n)
  # recursive method
  if(method == "recursive") {
    for(i in 1:n) {
      #M[,i] <- computeM(x,k,t,i)
    }   
  }
  # method via the formula Bi = (t[i+k]-t[i])Mi/k
  else if(method == "bsplines") {
    #B <- bsplineS(x, breaks=bs$breaks, norder=bs$norder)
    for(i in 1:n) {
      #M[, i] <- k * B[, i] / (t[i+k] - t[i])
    }
  }
  else
    stop("Error in 'getMSplineDesign' function.\n")
  
  return(M)
}

getISplineDesign <- function(bs, x)
{
  tmp.bs <- createISplineBasis(bs$breaks, bs$norder+1)
  tmp.M <- getISplineDesignTmp(tmp.bs, x)
  
  return(tmp.M[, -1])
}

getISplineDesignTmp <- function(bs, x)
{
  # process parameters
  x <- as.numeric(x)  
  
  # Prepare data  
  n <- bs$nbasis; k <- bs$norder-1
  t <- bs$knots
  rangeval <- bs$rangeval
  
  # Prepare local funtion
  getj <- function(x,t) {
    n <- length(x)
    nt <- length(t)
    jj <- array(,n)
    for(i in 1:n) {
      xx <- x[i]
      j <- which(sapply(1:(nt-1),function(ind) (t[ind] <= xx && xx < t[ind+1])))
      if(length(j) == 1)
        jj[i] <- j
      else 
        stop("Error in 'getj' funtion called from 'getISplineDesign'",
        "for x[i] value ",xx,".\n")     
    }
    return(jj)
  }
  
  # Compute design
  jj <- getj(x,t)
  unique.jj <- unique(jj)
  
  Mbs <- createMSplineBasis(bs$breaks, k+1)
  M <- matrix(0, nrow=length(x), ncol=n)
    
  for(i in 1:n) {    
    for(uj in 1:length(unique.jj)) {
      j <- unique.jj[uj]
      ind <- which(jj == j)      
      xj <- x[ind]
            
      Mi <- matrix(,nrow=length(ind),ncol=1)      
      if(i > j) 
        Mi <- 0
      else if(i < j-k+1)
        Mi <- 1
      else {
        # sum{M_m}
        MM <- getMSplineDesign(Mbs, xj)
        Mi <- 0
        for(m in i:j) {
          Mi <- Mi + (t[m+k+1]-t[m])*MM[,m]/(k+1)
        }        
      }   
      M[ind,i] <- Mi        
    }
  }
  
  return(M)
}

getIQSplineDesign <- function(bs, x)
{
  # process parameters
  xp <- as.numeric(x)  
  
  # Prepare data 
  n <- length(xp) 
  k <- bs$norder+1
  knots <- bs$knots
  nknots <- length(knots)
  nbasis <- bs$nbasis

  Xless <- sapply(knots, function(t, xp) xp < t, xp)
  Xmore <- sapply(knots, function(t, xp) xp >= t, xp)

  if(is.null(dim(Xless))) Xless <- matrix(Xless, nrow=1)
  if(is.null(dim(Xmore))) Xmore <- matrix(Xmore, nrow=1)  

  M <- sapply(2:(nbasis+1), function(i, xp, n, nbasis, knots, Xmore, Xless)
  {
    m <- rep(0, n)

    if(i > 1) {
      ind.fun <- Xmore[, i] & Xless[, i+1]
      if(sum(ind.fun)) {
        xt <- knots[i+1] - xp[ind.fun]
        tt <- (knots[i+1] - knots[i]) * (knots[i+1] - knots[i-1])
        m[ind.fun] <- 1 - xt * xt / tt
      }
    }
    
    if(i <= nbasis+1) {
      i <- i-1
      ind.fun <- Xmore[, i] & Xless[, i+1]
      if(sum(ind.fun)) {
        xt <- xp[ind.fun] - knots[i]
        tt <- (knots[i+1] - knots[i]) * (knots[i+2] - knots[i])
        m[ind.fun] <- m[ind.fun] + xt * xt / tt
      }
    }

    ind.one <- Xmore[, i+2]
    m[ind.one] <- 1
    
    return(m)
  }, xp, n, nbasis, knots, Xmore, Xless)
  
  return(M)
}

#----------------------------
# Functions 'fit'
#----------------------------

Penalty.matrix <- function (m, order = 2) 
{
    Diff.matrix <- function(m, order = 2) {
        d.matrix <- function(m) {
            A <- cbind(diag(m - 1), rep(0, m - 1))
            B <- cbind(rep(0, m - 1), -1 * diag(m - 1))
            d <- A + B
            return(d)
        }
        D <- d.matrix(m)
        if (order > 1) {
            for (k in 2:order) {
                D <- d.matrix(m - k + 1) %*% D
            }
        }
        return(D)
    }
    p <- length(m)
    start.block = cumsum(m) - m + 1
    end.block = cumsum(m)
    P <- matrix(0, sum(m), sum(m))
    for (i in 1:p) {
        D <- Diff.matrix(m[i], order = order)
        K <- t(D) %*% D
        P[start.block[i]:end.block[i], start.block[i]:end.block[i]] = K
    }
    return(P)
}


#' Function fitSplineBasis.
#'
#' @param bs An object of class \code{SplineBasis}.
#' @param y Numeric vector of data points (respone).
#' @return Numeric vector of beta coefficients.
#'
#' @name fitSplineBasis
#' @rdname SplineBasis-class
#' @export
fitSplineBasis <- function(bs, x, y)
{
  # matricies
  X <- getSplineDesign(bs, x)
  X <- X[, bs$ibasis]
  Y <- matrix(y, ncol=1)
    
  # regression
	XTX <- t(X) %*% X
  D <- Penalty.matrix(ncol(X), 2)
  if(sum(D == 0) == prod(dim(D)))
    stop("Error in 'fitSplineBasis': penalty matrix 'D' is zero-entry.")
    
  dvec <- as.vector(t(X) %*% Y)    
	Dmat <- XTX + bs$lambda * t(D) %*% D    
  
  # solver
  if(bs$type %in% c("ispline", "iqspline")) {
    Amat <- diag(1, ncol(X)) # nonneg. constraints
    
	  qpfit <- solve.QP(Dmat=Dmat, dvec=dvec, Amat=Amat, meq=0, factorized=FALSE)
    beta <- qpfit$solution
    
    tol <- 1e-10
    ind <- which(beta < 0 & abs(beta) < tol)
    beta[ind] <- 0
    if(sum(beta < 0))
      stop("Error in 'fitSplineBasis': some coeff. 'beta' are negative for type 'ispline'.")    
  }
  else {
    beta <- solve(Dmat, dvec, tol=bs$tol)
  }

  # complete beta
  beta <- c(beta, rep(0, bs$nbasis - bs$ncoef))
  
  return(beta)    
}

#' @S3method predict SplineBasis
predict.SplineBasis <- function(object, x, beta, ...)
{
  bs <- object
  
  # matricies
  X <- getSplineDesign(bs, x)
  y <- X %*% beta
    
  return(y)    
}
