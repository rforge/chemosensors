
#' @include ChemosensorsClass.R
NULL

#----------------------------
# ComputeAffinity Methods
#----------------------------

#' @rdname compute-methods
#' @aliases computeAffinity,ANY-method
setMethod("computeAffinity", "ANY", function(object, method, norm, ...)
{
  if(missing(method)) method <- "integral"
  if(missing(norm)) norm <- "none"
  
  aff <- switch(method,
    "integral" = computeAffinity.integral(object, ...),
    "inverse" = computeAffinity.inverse(object, ...),
    stop("Error in switch."))
  stopifnot(class(aff) == "matrix")
  
  aff <- switch(norm,
    "none" = aff,
    "norm" = {
      aff.norm <- as.numeric(apply(aff, 1, function(x) sqrt(sum(x*x))))
      aff / aff.norm
    },
    stop("Error in switch."))
    
    return(aff)
})

computeAffinity.integral <- function(object, cores)
{
  if(missing(cores)) cores <- 1
  
  nsensors <- nsensors(object)
  ngases <- ngases(object)
  gases <- gases(object)
  concMin <- rep(0, ngases)
  concMax <- concMax(object)
  
  nsd(object) <- 0
  enableDyn(object) <- FALSE
    
  affinity <- matrix(0, nrow = nsensors, ncol = ngases)
  
  parallel <- (cores > 1)
  if(parallel) {
    if(!require(doMC)) {
      stop("Package `doMC` is needef for parallel computation.")
    }
    registerDoMC(cores)
  }
  
  for(i in 1:ngases) {
    # define 'crange'  
    crange <- c(concMin[i], concMax[i])
  
    aff <- laply(1:nsensors, function(s) 
    {
      sensor <- getSensor(object, s)
      
      # local function
      f <- function(x) 
      {
        conc <- matrix(0, nrow = length(x), ncol = ngases)
        conc[, i] <- x
      
        y <- predict(sensor, conc)
        return(as.numeric(y))
      }

      # compute not-normalized 'affinity'      
      I <- integrate(f, crange[1], crange[2])
      val <- I$value
       
      # normalize affinity
      conc <- matrix(0, nrow = 2, ncol = ngases)
      conc[, i] <- crange
      yrange <- as.numeric(predict(sensor, conc))
      stopifnot(yrange[2] > yrange[1])
      
      val <- val / ((yrange[2] - yrange[1]) * (crange[2] - crange[1]))
     
      return(val)
    }, .parallel = parallel)
    
    affinity[, i] <- aff
  }
  
  # names
  colnames(affinity) <- gnames(object)
  rownames(affinity) <- snames(object)
  
  return(affinity)
}

computeAffinity.inverse <- function(object, cores)
{
  if(missing(cores)) cores <- 1
  
  nsensors <- nsensors(object)
  ngases <- ngases(object)
  gases <- gases(object)
  concMax <- concMax(object)
  
  nsd(object) <- 0
  enableDyn(object) <- FALSE
    
  affinity <- matrix(0, nrow = nsensors, ncol = ngases)
  
  conc <- matrix(0, nrow = ngases, ncol = ngases)
  for(i in 1:ngases) {
    conc[i, i] <- concMax[i]
  }
  
   sdata <- predict(object, conc, nclusters = cores)
   aff <- t(sdata)
   
   return(aff)
}


