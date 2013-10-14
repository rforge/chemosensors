#-------------------
# Temporary loading
#-------------------

#-------------------
# Dataset loading
#-------------------

#' Function loadUNIMANdata.
#'
#' @param dataset Name of dataset to be loaded.
#'
#' @name loadUNIMANdata
#' @rdname chemosensors-package
#' @export
loadUNIMANdata <- function(dataset)
{
  #stopifnot(exists(dataset))
  #eval(parse(text = paste("dat <-", dataset))) # -> `dat`
  
  # case 1: `dataset` exists (`devtools::load_all("src")`)

  if(!exists(dataset)) {
    # case 2: package `chemosensors` (returned value of `defaultDataPackage()`) is installed
    try(data(list = dataset, package = defaultDataPackage(), envir = environment()), silent = TRUE)
    if(!exists(dataset)) {
      # case 3: look through all the (a) packages and (b) local folders installed (see `?data`)
      try(data(list = dataset, package = NULL, envir = environment()), silent = TRUE)
      
      # case 4: failure
      if(!exists(dataset)) {
        stop("Error in `loadUNIMANdata`: dataset `", dataset, "` not found.\n")
      }
    }
  }    
  
  out <- list()  
  eval(parse(text = paste("out$dat <-", dataset))) # -> `dat`
  
  return(out$dat)
}

#-------------------
# Package Management
#-------------------

# @ http://r.789695.n4.nabble.com/test-if-a-package-is-installed-td1750671.html
is.installed <- function(mypkg) is.element(mypkg, installed.packages()[, 1]) 

#----------------------------
# Captured Variance
#----------------------------

capturedVar <- function(X, pc, mod) 
{
  if(missing(mod)) stop("Error in 'capturedVar': 'mod' is missing.")

  if(mod$center[1]) X <- as.matrix(sweep(X, 2, mod$center))  
  if(mod$scale[1])  X <- as.matrix(sweep(X, 2, mod$scale, "/"))
  
  E <- mod$rotation[, pc]
    
  capturedVarDir(X, E)
}

capturedVarDir <- function(X, E) 
{
  X <- as.matrix(X)  
  E <- as.matrix(E)
    
  var.projected <- apply(E, 2, function(e, X) sum((X %*% e)^2), X)
  var.total <- sum(apply(X, 2, function(x) sum((x)^2)))

  var.projected / var.total  
}

#----------------------------
# Other Functions
#----------------------------
lx <- function(x, c) ifelse(x < c, c - x, 0)
rx <- function(x, c) ifelse(x < c, 0, x - c)  

lX <- function(X, c) sapply(1:ncol(X), function(i, X, c) ifelse(X[, i] < c[i], c[i] - X[, i], 0), X, c)
rX <- function(X, c) sapply(1:ncol(X), function(i, X, c) ifelse(X[, i] < c[i], 0, X[, i] - c[i]), X, c)

