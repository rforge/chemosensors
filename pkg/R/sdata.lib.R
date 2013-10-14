
#' @include ChemosensorsClass.R
NULL

#----------------------------
# Extract Methods
#----------------------------
setMethod("extractSdata", "ANY", function(object, conc, sdata, sf, df, concUnits = "default", ...)
{
  if(concUnits == "default") concUnits <- defaultConcUnits()
  
  tunit <- tunit(object)
  
  # args
  args <- as.list(match.call())
  args <- args[-1]   
  
  # conc
  conc <- do.call("extractConc", args)
  
  # sdata
  if(!missing(sdata)) {
    sdata <- sdata
  }
  else if(!missing(sf)) {
    stopifnot(all(snames(object) %in% names(sf)))
    sdata <- subset(sf, select = snames(object), drop = FALSE)
    sdata <- as.matrix(sdata)
  }  
  else if(!missing(df)) {
    stopifnot(all(snames(object) %in% names(df)))
    sdata <- subset(df, select = snames(object), drop = FALSE)
    sdata <- as.matrix(sdata)
  }  
  else if(!missing(conc)) {
    sdata <- predict(object, conc, concUnits = concUnits, ...)
  }
  else {
    stop("Error in extractSdata: missing arguments.")
  }
  
  return(sdata)
})

#----------------------------
# Sdata Methods
#----------------------------

#' Method sdata2feature
#'
#' Method sdata2feature converts a matrix of sensor data  into a data frame of features.
#'
#' The input parameters are an object, e.g. of class \code{SensorArray}, a concentration matrix,
#' a matrix of sensor data, and (optionally) a data frame derived from conccentraion matrix.
#' 
#' @example inst/examples/sdata2feature-method.R
#' @rdname scenario-methods
#' @aliases sdata2feature,ANY-method
setMethod("sdata2feature", "ANY", function(object, conc, sdata, feature = "transient", 
  cf, sf, ...)
{ 
  stopifnot(!missing(object))

  tunit <- tunit(object)
  
  # 'feature'
  match.arg(feature, c("transient", "steady-state", "ss", "step"))
  if(feature == "ss") feature <- "steady-state"
  
  # form 'conc' and 'sdata'
  if(missing(conc)) {
    if(missing(cf)) 
      stop("Error in ANY::sdata.frame: both 'conc' and 'cf' are missing.")
    else {
      stopifnot(all(gnames(object) %in% names(cf)))
      conc <- subset(cf, select = gnames(object), drop = FALSE)
      conc <- as.matrix(conc)
    }
  }
  
  if(missing(sdata)) {
    if(missing(sf)) 
      stop("Error in ANY::sdata.frame: both 'sdata' and 'sf' are missing.")
    else {
      stopifnot(all(snames(object) %in% names(sf)))
      sdata <- subset(sf, select = snames(object), drop = FALSE)
      sdata <- as.matrix(sdata)
    }
  }

  stopifnot(nrow(conc) == nrow(sdata))
  
  # form 'cf' and 'sf'
  if(missing(cf)) {
    cf <- conc2df(object, conc, ...)
  }
  if(missing(sf)) {
    sf <- sdata2df(object, sdata, ...)
  }
  
  stopifnot(nrow(cf) == nrow(sf))

  ### variables
  nsamples <- nrow(conc)

  # check 'conc' is OK
  conc.ok <- checkConc(object, conc)
  if(!conc.ok)
    stop("Error in ANY::sdata2feature: concentration matrix 'conc' is incorrect.")

  airin <- getTPoint(object, conc, "airin")
  airout <- getTPoint(object, conc, "airout")
  gasin <- getTPoint(object, conc, "gasin")
  gasout <- getTPoint(object, conc, "gasout")
    
  df <- switch(feature,
    "transient" = cbind(sf, cf),
    "steady-state" = {
      stopifnot(length(gasout) > 0)
      
      cf <- cf[gasout, , drop = FALSE]
      sf <- sf[gasout, , drop = FALSE]      
      
      cbind(sf, cf)
    },
    "step" = {
      stopifnot(length(gasout) > 0)
      stopifnot(length(gasout) == length(airin))
      stopifnot(all(gasout > airin))

      cf <- cf[gasout, , drop = FALSE]
      sf <- sf[gasout, , drop = FALSE] - sf[airin, , drop = FALSE]

      cbind(sf, cf)      
    },
    stop("Error in ANY::sdata2feature: switch."))
  
  return(df)
})


sdata2feature.df <- function(object, feature, df, ...)
{ 
  stopifnot(!missing(object))
  
  stopifnot(all(gnames(object) %in% colnames(df)))
  stopifnot(all(snames(object) %in% colnames(df)))
  
  conc <- as.matrix(subset(df, select = gnames(object)))
  sdata <- as.matrix(subset(df, select = snames(object)))
  
  tunit <- tunit(object)
  
  # 'feature'
  match.arg(feature, c("transient", "steady-state", "ss", "step"))
  if(feature == "ss") feature <- "steady-state"

  # check 'conc' is OK
  conc.ok <- checkConc(object, conc)
  if(!conc.ok)
    stop("Error in ANY::sdata2feature.df: concentration matrix 'conc' is incorrect.")

  airin <- getTPoint(object, conc, "airin")
  airout <- getTPoint(object, conc, "airout")
  gasin <- getTPoint(object, conc, "gasin")
  gasout <- getTPoint(object, conc, "gasout")
    
  df <- switch(feature,
    "transient" = df,
    "steady-state" = {
      stopifnot(length(gasout) > 0)
      
      df[gasout, , drop = FALSE]
    },
    "step" = {
      stopifnot(length(gasout) > 0)
      stopifnot(length(gasout) == length(airin))
      stopifnot(all(gasout > airin))

      df.gasout <- df[gasout, , drop = FALSE]
      df.airin <- df[airin, , drop = FALSE]
      
      df.gasout[, snames(object)] <- df.gasout[, snames(object)] - df.airin[, snames(object)]

      df.gasout
    },
    stop("Error in ANY::sdata2feature: switch."))
  
  return(df)
}

#' Method sdata2df
#'
#' Method sdata2df converts a matrix of sensor data into a data frame.
#'
#' The input parameters are an object, e.g. of class \code{SensorArray}, and a matrix of sensor data.
#' 
#' @example inst/examples/sdata2df-method.R
#' @rdname scenario-methods
#' @aliases sdata2df,ANY-method
setMethod("sdata2df", "ANY", function(object, sdata, ...)
{  
  if(missing(sdata))
    stop("Error in ANY::sdata2df: 'sdata' is missing.")

  nsensors <- nsensors(object)
  nsensors.ok <- !is.na(nsensors)
  nsensors <- ifelse(nsensors.ok, nsensors, ncol(sdata))
  stopifnot(nsensors == ncol(sdata))
    
  sf <- data.frame(sdata)
  if(nsensors.ok) 
    names(sf) <- snames(object, ...)
  else
    names(sf) <- paste("S", 1:nsensors, sep = "")
  
  return(sf)
})

#' @rdname scenario-methods
#' @aliases sdata2pulse,ANY-method
setMethod("sdata2pulse", "ANY", function(object, conc, sdata, ...)
{
  if(missing(conc)) stop("Error in ANY::sdata2pulse: 'conc' is missing.")
  if(missing(sdata)) stop("Error in ANY::sdata2pulse: 'sdata' is missing.")

  ngases <- ngases(object)
  nsensors <- nsensors(object)
  n <- nrow(conc)
  
  tunit <- tunit(object)
  
  nsdata <- matrix(0, nrow=n, ncol = ngases)
  for(i in 1:ngases) {
    conci <- conc
    conci[, -i] <- 0
  
    dfi <- conc2df(object, conci)
    tpointi <- dfi$tpoint
    
    ind.gasin <- which(tpointi == "gasin")
    ind.gasout <- which(tpointi == "gasout")
    if(length(ind.gasin) != length(ind.gasout))
      stop("Error in ANY::sdata2pulse: incorrect format of concentration matrix 'conc'.\n",
        "Ppoints 'gasin' and 'gas.out' have different length.\n",
        "Check the concentration matrix 'conc' for gas #", i, ".", sep = "")    
    
    nk <- length(ind.gasin)
    if(nk) { # if nk > 0
      for(k in 1:nk) {
        nsdata[seq(ind.gasin[k], ind.gasout[k]), i] <- sdata[ind.gasout[k], i]
      }
    }
  }
  
  return(nsdata)
})








