
#----------------------------
# Captured Variance
#----------------------------

#' @export
capturedVar <- function(X, pc, mod) 
{
  if(missing(mod)) stop("Error in 'capturedVar': 'mod' is missing.")

  if(mod$center[1]) X <- as.matrix(sweep(X, 2, mod$center))  
  if(mod$scale[1])  X <- as.matrix(sweep(X, 2, mod$scale, "/"))
  
  E <- mod$rotation[, pc]
    
  capturedVarDir(X, E)
}

#' @export
capturedVarDir <- function(X, E) 
{
  X <- as.matrix(X)  
  E <- as.matrix(E)
    
  var.projected <- apply(E, 2, function(e, X) sum((X %*% e)^2), X)
  var.total <- sum(apply(X, 2, function(x) sum((x)^2)))

  var.projected / var.total  
}
