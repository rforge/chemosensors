# last revision: 9 Jan 2012

#' @include ChemosensorsClass.R
NULL

#----------------------------
# Sub Classes
#----------------------------

#----------------------------
# Parameters
#----------------------------

#' Function to get available names for concentration units.
#'
#' @name ConcUnitsNames
#' @rdname chemosensors-package
#' @return Character vector of units names.
#' @export
ConcUnitsNames <- function()
{
  return(c("perc", "perc 1e-2", "norm"))
}

ConcUnitsStr <- function(units)
{
  unitsStr <- switch(units,
    "perc" = "vol. %",
    "perc 1e-2" = "1e-2 vol. %",
    "norm" = "n.u.",
    stop("Error in concUnitsStr: 'units' is unknown."))

  return(unitsStr)
}

#----------------------------
# Defaults 
#----------------------------

#' Function defaultDataModel.
#' @name defaultDataModel
#' @rdname chemosensors-package
#' @export
defaultDataModel <- function() return("ispline")

#' Function defaultConcUnitsInt.
#' @name defaultConcUnitsInt
#' @rdname chemosensors-package
#' @export
defaultConcUnitsInt <- function() return("perc 1e-2")

#' Function defaultConcUnits.
#' @name defaultConcUnits
#' @rdname chemosensors-package
#' @export
defaultConcUnits <- function() return("perc")

#' Function defaultConcUnitsSorption.
#' @name defaultConcUnitsSorption
#' @rdname chemosensors-package
#' @export
defaultConcUnitsSorption <- function() return("norm")

#' Function defaultSet
#' @name defaultSet
#' @rdname chemosensors-package
#' @export
defaultSet <- function() return(c("A 0.01", "A 0.02", "A 0.05",
  "B 0.01", "B 0.02", "B 0.05",
  "C 0.1", "C 1"))

#----------------------------
# Defaults (Datasets)
#----------------------------

#' Function defaultDataPackage.
#' @name defaultDataPackage
#' @rdname chemosensors-package
#' @export
defaultDataPackage <- function() return("chemosensors") # "chemosensors", NULL

#' Function defaultDataSensorModel.
#' @name defaultDataSensorModel
#' @rdname chemosensors-package
#' @export
defaultDataSensorModel <- function() return("UNIMANshort")

#' Function defaultDataDistr.
#' @name defaultDataDistr
#' @rdname chemosensors-package
#' @export
defaultDataDistr <- function() return("UNIMANdistr")

#' Function defaultDataSensorNoiseModel.
#' @name defaultDataSensorNoiseModel
#' @rdname chemosensors-package
#' @export
defaultDataSensorNoiseModel <- function() return("UNIMANsnoise")

#' Function defaultDataSorptionModel.
#' @name defaultDataSorptionModel
#' @rdname chemosensors-package
#' @export
defaultDataSorptionModel <- function() return("UNIMANsorption")

#' Function defaultDataDriftNoiseModel.
#' @name defaultDataDriftNoiseModel
#' @rdname chemosensors-package
#' @export
defaultDataDriftNoiseModel <- function() return("UNIMANdnoise")

#----------------------------
# Export Methods
#----------------------------

#----------------------------
# Convert Methods
#----------------------------

#----------------------------
# Get/Set Methods
#----------------------------

#' @rdname get-methods
#' @aliases num,ANY-method
setMethod("num", "ANY", function(x) x@num)

setMethod("numStr", "ANY", function(x) {
  num <- x@num
  n <- length(num)
  numStr <- ifelse(n <= 5, 
    paste(num, collapse=", "), 
    paste(paste(num[1:3], collapse=", "), " ... ", num[n], sep=''))
  return(numStr)
})
#' @rdname get-methods
#' @aliases idx,ANY-method
setMethod("idx", "ANY", function(x) x@idx)
#' @rdname get-methods
#' @aliases gases,ANY-method
setMethod("gases", "ANY", function(x) x@gases)
#' @rdname get-methods
#' @aliases gind,ANY-method
setMethod("gind", "ANY", function(x) x@gind)
#' @rdname get-methods
#' @aliases ngases,ANY-method
setMethod("ngases", "ANY", function(x) ifelse("ngases" %in% slotNames(x), x@ngases, NA))
#' @rdname get-methods
#' @aliases gnames,ANY-method
setMethod("gnames", "ANY", function(x) x@gnames)

#' @rdname get-methods
#' @aliases gases,missing-method
setMethod("gases", "missing", function(x) 1:3)
#' @rdname get-methods
#' @aliases gind,missing-method
setMethod("gind", "missing", function(x) 1:3)
#' @rdname get-methods
#' @aliases ngases,missing-method
setMethod("ngases", "missing", function(x) 3)
#' @rdname get-methods
#' @aliases gnames,missing-method
setMethod("gnames", "missing", function(x) LETTERS[1:3])

#' @rdname get-methods
#' @aliases nsensors,ANY-method
setMethod("nsensors", "ANY", function(x) ifelse("num" %in% slotNames(x), length(x@num), NA))
#' @rdname get-methods
#' @aliases snames,ANY-method
setMethod("snames", "ANY", function(x, sensor.names = "short", ...) {
  match.arg(sensor.names, c("short", "long"))
  
  if("num" %in% slotNames(x) & "idx" %in% slotNames(x))
    return(switch(sensor.names,
      "long" = paste("S", x@idx, ", num ", x@num, sep = ""), # looks like: "S1, num 1", ...
      "short" = paste("S", x@idx, sep = ""))) 
  else
    return(NA)
})
#' @rdname get-methods
#' @aliases concUnits,ANY-method
setMethod("concUnits", "ANY", function(x) x@concUnits)
#' @rdname get-methods
#' @aliases concUnitsInt,ANY-method
setMethod("concUnitsInt", "ANY", function(x) x@concUnitsInt)


#' @name concUnits<-
#' @aliases concUnits<-,ANY-method
#' @rdname set-methods
setReplaceMethod("concUnits", "ANY", function(object, value) 
{
  object@concUnits <- value
  validObject(object)
  return (object)
})

#----------------------------
# Export Methods
#----------------------------
#' Method sdata.frame
#'
#' Method sdata.frame converts a concetration matrix and 
#' (optionally) a sensor data matrix into a data frame.
#'
#' The input parameters are an object, e.g. \code{SensorArray}, a concentration matrix,
#' and (optionally) a sensor data matrix.
#' The output data frame has the following columns:
#'
#' \tabular{rl}{
#'   \code{S1}, \code{S2}, ... \tab Sensor signals. \cr
#'   \code{A}, \code{B}, ... \tab Gas concentrations (column names equal to gas names of the object). \cr
#'   \code{glab} \tab Gas labels, e.g. \code{A} or \code{Air}. \cr
#'   \code{lab} \tab Gas+Concetratoin labels, e.g. \code{A 0.01}. \cr
#'   \code{tpoint} \tab Time point labels to encode the gas pulses, e.g. \code{gasin}.
#' }
#' 
#' @rdname scenario-methods
#' @aliases sdata.frame,ANY-method
#' @example inst/examples/sdata.frame-method.R
setMethod("sdata.frame", "ANY", function(x, feature, df, ...) 
{ 
  stopifnot(!missing(x))
  stopifnot(!missing(feature))
  
  if(missing(df)) {
    of <- sdata2feature(x, feature = feature, ...)
  } 
  else {
    of <- sdata2feature.df(x, feature = feature, df = df, ...)
  }
  
  return(of)
})

#----------------------------
# Predict Methods
#----------------------------

#' @rdname get-methods
#' @aliases coef,ANY-method
setMethod("coef", "ANY", function(object, ...)
{
  coefficients(object, ...)  
})

setMethod("coefStr", "ANY", function(object, sensor = 1, ...)
{
  coef <- as.numeric(round(coef(object, ...)[, sensor], 4)) # take the first column
  n <- length(coef)
  
  coefStr <- ifelse(n <= 5, 
    paste(coef, collapse=", "), 
    paste(paste(coef[1:3], collapse=", "), " ... ", coef[n], sep=''))
  return(coefStr)
  
})

#----------------------------
# Compute Methods
#----------------------------

#----------------------------
# Model Methods
#----------------------------

#' @rdname model-methods
#' @aliases concMin,ANY-method
setMethod("concMin", "ANY", function(object, concUnits=object@concUnits, ...)
{
  conc.min.perc <- c(0.01, 0.01, 0.1)[gases(object)]
  conc.min <- switch(concUnits,
    "perc" = conc.min.perc,
    "perc 1e-2" = 100 * conc.min.perc,
    "norm" = conc.min.perc / concMax(object, concUnits="perc"),  
    stop("Error in ANY::concMin: 'concUnits' is not supported."))
  
  return(conc.min)
})

#' @rdname model-methods
#' @aliases concMax,ANY-method
setMethod("concMax", "ANY", function(object, concUnits=object@concUnits, ...)
{
  conc.max.perc <- c(0.1, 0.1, 1.0)[gases(object)]
  conc.max <- switch(concUnits,
    "perc" = conc.max.perc,
    "perc 1e-2" = 100 * conc.max.perc,
    "norm" = c(1, 1, 1)[gases(object)],
    stop("Error in ANY::concMax: 'concUnits' is not supported."))
  
  return(conc.max)
})


### Method concNorm
setMethod("concNorm", "ANY", function(object, conc, concUnits="default", concUnitsInt="default", ...)
{
  if(missing(conc)) 
    stop("Error in ANY::concNorm: missing 'conc'.")  
  if(concUnits == "default") concUnits <- concUnits(object)
  if(concUnitsInt == "default") concUnitsInt <- concUnitsInt(object)

  if(!(concUnits %in% ConcUnitsNames())) # from 'ChemosensorsClassMethods.R'
    stop("Error in ANY::concNorm: 'concUnits' is incorrect.")
  
  conc <- switch(concUnitsInt,
     "perc 1e-2" = switch(concUnits,
        "perc" = 100 * conc,
        "perc 1e-2" = conc,   
        "norm" =  sweep(conc, 2, concMax(object, concUnits=concUnitsInt), "*"),                  
        stop("Error in ANY::concNorm: 'concUnits' is not supported for 'concUnitsInt' equal to 'perc 1e-2'.")),
     "perc" = switch(concUnits,
        "perc" = conc,
        "perc 1e-2" = 0.01 * conc,   
        "norm" =  sweep(conc, 2, concMax(object, concUnits=concUnitsInt), "*"),                  
        stop("Error in ANY::concNorm: 'concUnits' is not supported for 'concUnitsInt' equal to 'perc'.")),
     "norm" = sweep(conc, 2, concMax(object, concUnits=concUnits), "/"),
     stop("Error in ANY::concNorm: 'concUnitsInt' (", concUnitsInt, ") is not supported."))
    
  return(conc)
})

### Method concNorm
setMethod("concNorm", "missing", function(object, conc, concUnits, concUnitsInt, ...)
{
  if(missing(conc)) 
    stop("Error in missing::concNorm: missing 'conc'.")  
  if(missing(concUnits)) concUnits <- defaultConcUnits()
  if(missing(concUnitsInt)) concUnitsInt <- defaultConcUnitsInt()
  
  conc <- switch(concUnitsInt,
     "perc 1e-2" = switch(concUnits,
        "perc" = 100 * conc,
        "perc 1e-2" = conc,
        "norm" =  sweep(conc, 2, concMax(object, concUnits=concUnitsInt), "*"),     
        stop("Error in ANY::concNorm: 'concUnits' is not supported for 'concUnitsInt' equal to 'perc 1e-2'.")),
     "norm" = sweep(conc, 2, concMax(object, concUnits=concUnits), "/"),
     stop("Error in ANY::concNorm: slot 'concUnitsInt' is not supported."))
         
  return(conc)
})

### Method concDenorm
setMethod("concDenorm", "ANY", function(object, conc, concUnits="default", concUnitsInt="default", ...)
{
  if(missing(conc)) 
    stop("Error in ANY::concDenorm: missing 'conc'.")  
  if(concUnits == "default") concUnits <- concUnits(object)
  if(concUnitsInt == "default") concUnitsInt <- concUnitsInt(object)  
  
  if(!(concUnits %in% ConcUnitsNames())) # from 'ChemosensorsClassMethods.R'
    stop("Error in ANY::concDenorm: 'concUnits' is incorrect.")
  
  conc <- switch(concUnits,
     "perc" = switch(concUnitsInt,
        "perc 1e-2" = 0.01 * conc,
        "perc" = conc,
        "norm" = sweep(conc, 2, concMax(object, concUnits=concUnits), "*"),   
        stop("Error in ANY::concDenorm: 'concUnitsInt' (", concUnitsInt, ") is not supported for 'concUnits' equal to 'perc'.")),
     "perc 1e-2" = switch(concUnitsInt,
        "perc 1e-2" = conc,
        "perc" = 100 * conc,
        "norm" = sweep(conc, 2, concMax(object, concUnits=concUnits), "*"),   
        stop("Error in ANY::concDenorm: 'concUnitsInt' (", concUnitsInt, ") is not supported for 'concUnits' equal to 'perc'.")),
     stop("Error in ANY::concDenorm: 'concUnits' (", concUnits, ") is not supported."))
    
  return(conc)
})

#' @rdname model-methods
#' @aliases concSample,ANY-method
setMethod("concSample", "ANY", function(object, type, n, 
  gases=0, concUnits="default", ...)
{
  tval <- c("const", "inc", "range", "mixture")
  
  if(missing(n)) n <- 100
  if(missing(type)) type <- "const"

  if(sum(gases == 0)) gases <- gases(object)
  if(concUnits == "default") concUnits <- concUnits(object)

  ngases <- ngases(object) 
  gind <- gind(object)
  
  # create 'conc' ('perc' units)
  # n samples per gas
  m <- concMin(object, concUnits=concUnits)
  M <- concMax(object, concUnits=concUnits)
  cgases <- c(0.02, 0.02, 0.2) 
  cgases <- cgases[gases]

  if(type == "const") {
    conc <- matrix(0, nrow= n * length(gases), ncol=ngases) 
    for(i in 1:length(gases)) {
      g <- gases[i] 
      gi <- gind[g] 
      ind <- 1:n + (i - 1) * n
      conc[ind, gind[g]] <- cgases[gi]
    }
  }
  else if(type == "inc") {
    conc <- matrix(0, nrow= n * length(gases), ncol=ngases) 
    for(i in 1:length(gases)) {
      g <- gases[i] 
      gi <- gind[g]   
      ind <- 1:n + (i - 1) * n
      conc[ind, gind[g]] <- seq(0, M[gi], length=n)
    }
  }
  else if(type == "range") {
    conc <- matrix(0, nrow= n * length(gases), ncol=ngases) 
    for(i in 1:length(gases)) {
      g <- gases[i] 
      gi <- gind[g] 
      ind <- 1:n + (i - 1) * n
      conc[ind, gind[g]] <- seq(m[gi], M[gi], length=n)
    }
  }  
  else if(type == "mixture") {
    if(length(gases) != 2)
      stop("Error in ANY::concSample: 'length(gases) != 2' for type 'mixture'.")    
    if(n > 50)      
      stop("Error in ANY::concSample: 'n > 10' for type 'mixture'.")          

    conc <- matrix(0, nrow= n * n, ncol=ngases) 
    for(i in 1:n) {
      g1 <- gases[1] 
      gi1 <- gind[g1]
      g2 <- gases[2]
      gi2 <- gind[g2]

      ii <- 1:n + (i - 1) * n
      #conc1 <- seq(0, M[gi1], length=n)
      #conc2 <- seq(0, M[gi2], length=n)[i]
      conc1 <- seq(m[gi1], M[gi1], length=n)
      conc2 <- seq(m[gi2], M[gi2], length=n)[i]
      
      conc[ii, gi1] <- conc1
      conc[ii, gi2] <- conc2
    }
  }   
  else 
    stop("Error in ANY::concSample: 'type' is unknown.")
  
  # convert 'conc' to '@concUnits'
  #conc <- switch(concUnits,
  #  'perc' = conc,
  #  'perc 1e-2' = 100 * conc,
  #   'norm' = sweep(conc, 2, concMax(object, concUnits=concUnits), "/"),    
  #  stop("Error in ANY::concSample: 'concUnits' is not supported."))
    
  return(conc)
})

#' @rdname model-methods
#' @aliases concSampleDyn,ANY-method
setMethod("concSampleDyn", "ANY", function(object, type, n, 
  gases = 0, concUnits = "default", ...)
{
  tval <- c("const")
  
  if(missing(n)) n <- 1
  if(missing(type)) type <- "const"

  if(sum(gases == 0)) gases <- gases(object)
  if(concUnits == "default") concUnits <- concUnits(object)

  # check par.
  if(n != 1)
    stop("Error in ANY::concSampleDyn: n != 1 (not supported yet).")

  ngases <- ngases(object) 
  gind <- gind(object)

  tunit <- tunit(object)
  
  # create 'conc' ('perc' units)
  # n samples per gas
  m <- concMin(object, concUnits=concUnits)
  M <- concMax(object, concUnits=concUnits)
  mM <- 0.5 * (m + M)

  np <- n * tunit # n 'pulse'
  nn <- np * 2 * ngases
  conc0 <- matrix(0, nrow= np, ncol=ngases)
  
  conc <- matrix(0, nrow= nn, ncol=ngases)
  
  if(type == "const") {
    for(i in 1:length(gases)) {
      g <- gases[i] 
      gi <- gind[g] 
      # resposne phase
      ind <- 1:np + (i - 1) * 2 * np
      conc[ind, gind[g]] <- mM[gi]
      # recovery phase
    }
  }
  else 
    stop("Error in ANY::concSampleDyn: 'type' is unknown.")
  
  return(conc)
})


setMethod("sdataSample", "ANY", function(object, n, ...)
{
  if(missing(n)) n <- 100
  nsensors <- nsensors(object)
  
  sdata.base <- matrix(seq(0, nsensors, length=nsensors), nrow=n, ncol=nsensors, byrow=TRUE)
  sdata.inc <- matrix(seq(2, 5, length=n), nrow=n, ncol=nsensors)
  sdata <- sdata.inc # + sdata.base

  # set names of 'coef'
  colnames(sdata) <- num(object)
  
  return(sdata)
})



#----------------------------
# Plot Methods
#----------------------------

#' @rdname plot-methods
#' @aliases plotPolar,ANY-method
setMethod("plotPolar", "ANY", function(x, y, polar = TRUE, geom = "line",
  main = "Model Response: Polar plot", xlab = "Sensor", ylab = "Sensor Signal", xlim = NULL, ylim = NULL,
  graphics = "ggplot", ret = TRUE, ...)
{
  # check par. 'graphics'
  match.arg(graphics, c("ggplot"))
  match.arg(geom, c("line", "area"))
  
  # object parameters
  nsensors <- nsensors(x)
  if(nsensors == 1)
    stop("Error in ANY::plotPolar: 'nsensors == 1'.")  
 
  gind <- gind(x)
  gnames <- gnames(x)
  ngases <- ngases(x)
  gcol <- gcol(x)[gind]

  # set up object
  nsd(x) <- 0
  enableDyn(x) <- FALSE
 
  # plot parameters
  
  # plot
  if(graphics == "ggplot") {
    options(stringsAsFactors = FALSE)
    
    # df: data-frame with variables.
    # $ y: sensor response.
    # $ S: sensor name (for plotting).
    # $ sensor: sensor index (for order).   
    # $ gas: gas (for facetting).
    # $ conc: concentration, for example, min/max (for line width).

    # create data frame
    cf <- data.frame(gind = gind, gas = gnames, 
      #middle = 0.5 * (concMax(x)[gind] + concMin(x)[gind]),
      max = concMax(x)[gind])
  
    cf <- melt(cf, id.vars=c("gind", "gas"), variable_name = "conc.level")
    colnames(cf)[ncol(cf)] <- "conc"
  
    conc <- t(apply(cf, 1, function(r) {conc <- rep(0, 3); conc[as.numeric(r["gind"])] <- as.numeric(r["conc"]); conc }))
  
    sdata <- predict(x, conc = conc)
    colnames(sdata) <- x@idx
  
    df <- cbind(cf, sdata)
    df <- melt(df, id.vars = colnames(cf))  
    colnames(df)[c(ncol(df) - 1, ncol(df))] <- c("sensor", "y")
    df <- mutate(df, S = paste("S", sensor, sep = ""))

    df <- mutate(df, sensor = as.numeric(sensor))
  
    ff <- subset(df, S == "S1") # fake frame
    ff$sensor <- nsensors + 1
    ff$S <- ""  
    df2 <- rbind(df, ff)

    yr <- range(df2$y)
    yr[1] <- min(yr[1], 0)

    if(geom == "line") {
      p <- ggplot(df2, aes(x = sensor, y = y, group = gas)) + ylim(yr)
      p <- p + geom_line(aes(color = gas)) + scale_color_manual(values = gcol) 
    }
    else if(geom == "area") {
      p <- ggplot(df2, aes(x = sensor, y = y, group = gas, fill = gas)) + scale_fill_manual(values = gcol)
      #p <- p + geom_area(position = "fill")
      p <- p + geom_area(position = "identity")
      #p <- p + facet_wrap(~ gas, ncol = 2)
    } 
    else
      stop("Error in ANY::plotPolar: 'geom' is incorrect.")  
    
    df3 <- subset(df2, gas == df2$gas[1])
    p <- p + scale_x_continuous(breaks = df3$sensor, labels = df3$S)
   
    p <- p +  labs(x = xlab, y = ylab, title = main)
    
    if(polar) {
      p <- p + coord_polar()
    }
    if(ret) {
      return(p)
    } 
    else {
      print(p)
      return(invisible())
    }
  }

})

#' @rdname plot-methods
#' @aliases plotPolarGases,ANY-method
setMethod("plotPolarGases", "ANY", function(x, y, polar = TRUE, geom = "line",
  main = "Model Response: Polar plot per analyte", xlab = "Sensor", ylab = "Sensor Signal", xlim = NULL, ylim = NULL,
  graphics = "ggplot", ret = FALSE, ...)
{
  # check par
  match.arg(graphics, c("ggplot"))
  match.arg(geom, c("line", "area"))
  
  # object parameters
  nsensors <- nsensors(x)
  if(nsensors == 1)
    stop("Error in ANY::plotPolarGases: 'nsensors == 1'.")  
 
  gind <- gind(x)
  gnames <- gnames(x)
  ngases <- ngases(x)
  gcol <- gcol(x)[gind]

  # set up object
  nsd(x) <- 0
  enableDyn(x) <- FALSE
 
  # plot parameters
  
  # plot
  if(graphics == "ggplot") {
    options(stringsAsFactors = FALSE)
    
    # df: data-frame with variables.
    # $ y: sensor response.
    # $ S: sensor name (for plotting).
    # $ sensor: sensor index (for order).   
    # $ gas: gas (for facetting).
    # $ conc: concentration, for example, min/max (for line width).

    # create data frame
    cf <- data.frame(gind = gind, gas = gnames, 
      max = concMax(x)[gind], min = concMin(x)[gind], min.quarter = concMin(x)[gind] / 4)
  
    cf <- melt(cf, id.vars=c("gind", "gas"), variable_name = "conc.level")
    colnames(cf)[ncol(cf)] <- "conc"
  
    conc <- t(apply(cf, 1, function(r) {conc <- rep(0, 3); conc[as.numeric(r["gind"])] <- as.numeric(r["conc"]); conc }))
  
    sdata <- predict(x, conc = conc)
    colnames(sdata) <- x@idx
  
    df <- cbind(cf, sdata)
    df <- melt(df, id.vars = colnames(cf))  
    colnames(df)[c(ncol(df) - 1, ncol(df))] <- c("sensor", "y")
    df <- mutate(df, S = paste("S", sensor, sep = ""))

    df <- mutate(df, sensor = as.numeric(sensor))
  
    ff <- subset(df, S == "S1") # fake frame
    ff$sensor <- nsensors + 1
    ff$S <- ""  
    df2 <- rbind(df, ff)

    yr <- range(df2$y)
    yr[1] <- min(yr[1], 0)

    #if(geom == "line") {
    #  p <- ggplot(df2, aes(x = sensor, y = y, group = conc.level)) + ylim(yr)
    #  #p <- p + geom_line()
    #  p <- p + geom_line(aes(size = conc.level)) + scale_size_manual(values = c(0.5, 0.3, 0.1))  
    #}
    #else if(geom == "area") {
    #  p <- ggplot(df2, aes(x = sensor, y = y, group = conc.level, fill = conc.level)) + scale_fill_grey()
    #  #p <- p + geom_area(position = "fill")
    #  p <- p + geom_line() + geom_area(position = "identity")
    #} 
    #else
    #  stop("Error in ANY::plotPolar: 'geom' is incorrect.")  
    #
    #p <- p + facet_wrap(~ gas, ncol = 2)
    #p <- p +  labs(x = xlab, y = ylab) + opts(title = main)
    #    
    #if(polar) {
    #  p <- p + coord_polar(theta = "x")
    #
    #  df3 <- subset(df2, gas == df2$gas[1])
    #  p <- p + scale_x_continuous(breaks = df3$sensor, labels = df3$S)
    #}
    
    if(ret) {
      return(p)
    } 
    else {
      print(p)
      return(invisible())
    }
  }

})

### Method plotResponse
setMethod("plotResponseOld", "ANY", function(x, y, concUnits = "norm", 
  jitter, 
  gases = 0, type = "inc", n = 100, 
  uniman = FALSE, affinity=FALSE, datasetSensorModel, pck, 
  lwd = 1, lty = 1, pch = 20, 
  xlim = NULL, ylim = NULL, 
  Ka,
  main = "Model Response", xlab = "Normalized Concentration", ylab="Signal", 
  graphics = "ggplot", ret = FALSE, ...)
{
  # check par. 'graphics'
  match.arg(graphics, c("base", "ggplot"))
  
  if(uniman) {
    if(missing(datasetSensorModel)) datasetSensorModel <- defaultDataSensorModel()
    if(missing(pck)) pck <- defaultDataPackage()
    if(missing(jitter)) jitter <- TRUE    
  }  
  if(missing(jitter)) jitter <- FALSE
  if(sum(gases == 0)) gases <- gases(x)
  
  if(missing(Ka)) Ka <- NULL
        
  gind <- gind(x)
  nsensors <- nsensors(x)
  ngases <- ngases(x)
  gnames <- gnames(x)
  
  nsd(x) <- 0
  enableDyn(x) <- FALSE
  
  # points
  yp <- matrix(NA, nrow = n, ncol = length(gases) * nsensors)
  xp <- matrix(NA, nrow = n, ncol = length(gases) * nsensors)
  for(i in 1:length(gases)) {
    g <- gases[i]  
    gi <- gind[g]
    
    conci <- concSample(x, type, n=n, gases=g)
    
    ind <- seq(1, nsensors) + (i - 1) * nsensors
    xpi <- concNorm(x, conc=conci, concUnitsInt=concUnits)[, gi]
    xp[, ind] <- xpi

    ypi <- predict(x, conc=conci)
    yp[, ind] <- ypi
  }
  
  # ylim
  if(uniman) {
    data(list=datasetSensorModel, package=pck, envir=environment()) # -> 'C', 'dat', 'dat.corrected'
    X <- dat

    if(is.null(ylim)) ylim <- range(range(X[, num(x)]), range(yp))
  }  
  
  if(is.null(xlim)) xlim <- range(xp)
  xlim[2] <- xlim[2]
  xlim[1] <- 0
  
  col <- gcol(x, gases=rep(gases, each=nsensors))  
  
  # compute affinity, if needed
  if(affinity) {
    yp.min <- min(yp)
    yp.max <- max(yp)
    yp.mid <- 0.5 * (yp.min + yp.max)
    
    if(is.null(Ka)) Ka <- affinity(x)
    Kd <- 1 / Ka
  }
    
  if(graphics == "base") {
    matplot(xp, yp, bty='n', t='l', col=col, 
      lwd = lwd, lty = lty,
      xlim = xlim, ylim=ylim, 
      main=main, xlab = xlab, ylab = ylab, ...)  
   }
  else if(graphics == "ggplot") {
    xf <- as.data.frame(xp)
    colnames(xf) <- paste(rep(1:nsensors, each = ngases), rep(1:ngases, times = nsensors), sep = ".")
    xm <- reshape::melt.data.frame(xf, measure.vars = colnames(xf))
    colnames(xm) <- c("id", "conc")

    yf <- as.data.frame(yp)
    colnames(yf) <- paste(rep(1:nsensors, each = ngases), rep(1:ngases, times = nsensors), sep = ".")
    ym <- reshape::melt.data.frame(yf, measure.vars = colnames(yf))
    colnames(ym) <- c("id", "sdata")

    dm <- cbind(xm, sdata = ym$sdata)
    tmp <- strsplit(as.character(dm$id), "\\.")
    dm$sensor <- paste("S", as.numeric(unlist(lapply(tmp, function(x) x[1]))), sep = "")
    dm$gas <- gnames[as.numeric(unlist(lapply(tmp, function(x) x[2])))]
    
    p <- qplot(conc, sdata, data = dm, geom = "line", group = id, color = gas) +
      scale_colour_manual(values = gcol(x)) + 
      labs(x = xlab, y = ylab, title = main)
    
    if(!is.null(xlim)) p <- p + xlim(xlim[1], xlim[2])
    if(!is.null(ylim)) p <- p + ylim(ylim[1], ylim[2])
        
    if(affinity) {
      col <- gcol(x)
      for(s in 1:ncol(Kd)) {
        for(i in 1:length(gases)) {
          g <- gases[i]  
          gi <- gind[g]
          
          p <- p +  ggplot2::geom_vline(xintercept = Kd[gi, s], colour = col[gi], linetype = 2)
        }
      }
     # p <- p + ggplot2::geom_vline(xintercept = Kd, color = gcol()
      #c(yp.min, yp.mid, yp.max)
       
      #  abline(h=, lwd=lwd, lty=3) # minimum/maximum and middle response values
       # abline(v=, col=gcol(x), lwd=lwd, lty=3) # concentration levels 'Kd'
  }
      
    if(ret) {
      return(p)
    } 
    else {
      print(p)
      return(invisible())
    }
  }
  
  if(uniman & graphics == "base") {
    num <- num(x)
        
    for(i in 1:length(gases)) {
      g <- gases[i]
      gi <- gind[g]
      ind <- (C[, g] != 0)
      
      xpi <- concNorm(x, conc=C[ind, gases(x)], concUnitsInt=concUnits)[, gi]
      xpi <- matrix(xpi, nrow=length(xpi), ncol=nsensors)
      ypi <- X[ind, num]
      
      if(jitter) xpi <- jitter(xpi, factor=0.2)  
      
      col <- gcol(x, gases=g)
      points(xpi, ypi, col=col, pch=pch)
    }
  }
})

#' @rdname plot-methods
#' @aliases plotTimeline,ANY-method
setMethod("plotTimeline", "ANY", function(x, y, conc, sdata, concUnits = "default",
  leg="none",
  lwd = 2, lty = 1, col, bty="n",
  main = "Model Response: Timeline", xlab, ylab, ...)
{
  if(concUnits == "default") concUnits <- concUnits(x)
  if(missing(conc))
    stop("Error in ANY::plotTimeline: 'conc' is missing.")

  nsensors <- nsensors(x)
  
  # sdata  
  if(missing(sdata)) sdata <- predict(x, conc, concUnits=concUnits)
  
  # plot parameters
  if(missing(xlab)) xlab <- paste("Time, ", "a.u.", sep='')
  if(missing(ylab)) ylab <- paste("Signal, ", "a.u.", sep='')
  
  if(missing(col)) col <- scol(x, nsensors)
  
  # plot
  matplot(sdata, t="l", lwd=lwd, lty=lty, bty=bty, 
    col=col,
    main=main, xlab = xlab, ylab = ylab, ...)  
  abline(h=0, lty=3, col='lightgrey', lwd=lwd)
  
  # legend
  if(leg != "none") {
    lab <- paste("S", 1:nsensors, sep='')
    legend(leg, legend=lab, col=col, lwd=lwd, lty=lty, bt="n")
  }
  
})

### Method plotAffinitySpaceOld
setMethod("plotAffinitySpaceOld", "ANY", function(x, y, type = 'points', conc, sdata,
  gases = 1:2, 
  lwd = 2, pch=20, cex=2, 
  xlim, ylim, 
  main = "Affinity Space", xlab, ylab, ...)
{
  # missing
  tval <- c('ellipse', 'points', 'density')
  match.arg(type, tval)
  
  # missing object
  missing.object <- missing(x)
  if(missing.object) {
    if(missing(conc) | missing(sdata))
      stop("Error in ANY::plotAffinitySpace: 'conc' and 'sdata' are missing.")
  }
  else {
    if(missing(conc)) conc <- concSample(x, ...)
    sdata <- predict(x, conc)
  }
  
  gind <- gases
  nsensors <- ifelse(missing.object, ncol(sdata), nsensors(x))
  gnames <- LETTERS[gind]

  if(nsensors < 2) 
    stop("Error in ANY::plotAffinitySpace: 'nsensors < 2'.")
  if(length(gind) != 2) {
    stop("Error in ANY::plotAffinity: 'length(gind) != 2'.")
  }
  
  # filter by 'gind'
  conc.max <- apply(conc, 2, max)
  ind <- which(apply(conc, 1, function(x, conc.max) sum(x == conc.max) > 0, conc.max))
  conc <- conc[ind, ]
  sdata <- sdata[ind, ]

  gcol <- apply(conc, 1, function(x) which(x != 0))
  class <- LETTERS[gcol]

  mu <- t(apply(sdata, 2, function(x, class) tapply(x, class, mean), class))
  sd <- t(apply(sdata, 2, function(x, class) tapply(x, class, sd), class)) 
  sd.max <- max(sd)

  xp <- mu[, gind[1]]
  yp <- mu[, gind[2]]
  
  ### plot
  if(missing(xlab)) xlab <- gnames[1]
  if(missing(ylab)) ylab <- gnames[2]
  
  if(missing(xlim)) xlim <- range(xp -  sd.max, xp +  sd.max)
  if(missing(ylim)) ylim <- range(yp -  sd.max, yp +  sd.max)  
    
  # plot ellipse
  if(type == 'ellipse') {
   plot(xp, yp, bty="n", type="n", 
     xlim=xlim, ylim=ylim,
     xlab=xlab, ylab=ylab, main=main, ...)
  
    col <- grey.colors(1, start=0.3)
    for(i in 1:nsensors) {
      varcovi <- diag(sd[i, gind])
      mui <- mu[i, gind]
      if(!require(car)) {
        stop("Package `car` is needed for plotting.")
      }
      car::ellipse(mui, varcovi, 1, col=col, center.cex=0, fill=TRUE, lwd=lwd)
    }
    abline(0, 1, lwd=lwd, lty=3) 
  }
  # plot points
  else if(type == 'points') {
    plot(xp, yp, bty="n", type="n", 
     xlim=xlim, ylim=ylim,
     xlab=xlab, ylab=ylab, main=main, ...)

    points(xp, yp, pch=pch, cex=cex)
    abline(0, 1, lwd=1, lty=3) 
    #grid(col='lightgrey', lwd=lwd, lty=3)
  }
  else if(type == 'density') {
    if(!require(MASS)) {
        stop("Package `MASS` is needed for plotting.")
    }  

    d <- MASS::kde2d(xp, yp, n=50)

    col <- mapcol()
  
    image(d, col=col,
      main=main, xlab=xlab, ylab=ylab, ...)
    contour(d, add=TRUE, nlevels=10)
  }
  else
    stop("Error in ANY::plotAffinitySpace: 'type' is unknow.")  
  
})

#' @rdname plot-methods
#' @aliases plotAffinitySpace,ANY-method
setMethod("plotAffinitySpace", "ANY", function(x, y, type = 'points', 
  gases = 1:2, 
  main = "Affinity Space", xlab, ylab, 
  graphics = "ggplot", geom = "point", ret = FALSE, ...)
{
  # missing
  match.arg(graphics, c("ggplot"))
  
  tval <- c('points')
  match.arg(type, tval)
  
  gind <- gind(x)
  gnames <- gnames(x)
  ngases <- ngases(x)
  gcol <- gcol(x)[gind]
  
  if(is.numeric(gases)) gases <- gnames[gases]

  nsensors <- nsensors(x)  

  if(nsensors < 2) 
    stop("Error in ANY::plotAffinitySpace: 'nsensors < 2'.")
  if(length(gases) != 2) {
    stop("Error in ANY::plotAffinity: 'length(gind) != 2'.")
  }
    
  # set up object
  nsd(x) <- 0
  enableDyn(x) <- FALSE
 
  # plot parameters
  if(missing(xlab)) xlab <- paste("Gas", gases[1])
  if(missing(ylab)) ylab <- paste("Gas", gases[2])  
  
  # plot
  if(graphics == "ggplot") {
    options(stringsAsFactors = FALSE)
    
    # df: data-frame with variables.
    # $ y: sensor response.
    # $ S: sensor name (for plotting).
    # $ sensor: sensor index (for order).   
    # $ gas: gas (for facetting).
    # $ conc: concentration, for example, min/max (for line width).

    # create data frame
    cf <- data.frame(gind = gind, gas = gnames, 
      middle = 0.5 * (concMax(x)[gind] + concMin(x)[gind]))
  
    cf <- melt(cf, id.vars=c("gind", "gas"), variable_name = "conc.level")
    colnames(cf)[ncol(cf)] <- "conc"
  
    conc <- t(apply(cf, 1, function(r) {conc <- rep(0, 3); conc[as.numeric(r["gind"])] <- as.numeric(r["conc"]); conc }))
  
    sdata <- predict(x, conc = conc)
    colnames(sdata) <- x@idx
  
    df <- cbind(cf, sdata)
    df <- melt(df, id.vars = colnames(cf))  
    colnames(df)[c(ncol(df) - 1, ncol(df))] <- c("sensor", "y")
    df <- mutate(df, S = paste("S", sensor, sep = ""))

    df <- mutate(df, sensor = as.numeric(sensor))
  
    # filter by gases
    stopifnot(length(gases) == 2)
    df <- subset(df, gas %in% gases)

    df1 <- subset(df, gas == gases[1])
    df2 <- subset(df, gas == gases[2])

    jf <- join(df1, df2, by = c("sensor", "S"))
    jf.names <- colnames(jf)
    colnames(jf)[jf.names == "y"] <- c("y1", "y2")

    rxy <- range(range(jf$y1), range(jf$y2))
    
    if(geom == "point") {
      p <- ggplot(jf, aes(x = y1, y = y2)) + xlim(rxy) + ylim(rxy)
      p <- p + geom_point() + geom_abline(intercept = 0, slope = 1) 
    }
    else if(geom == "tile") {
      p <- ggplot(jf, aes(x = y1, y = y2)) + xlim(rxy) + ylim(rxy)
      #p <- p + stat_density(aes(fill=..density..), geom="tile", position="identity")
    }
    else
      stop("Error in ANY::plotPolar: 'geom' is incorrect.")  

    p <- p +  labs(x = xlab, y = ylab, title = main)

    if(ret) {
      return(p)
    } 
    else {
      print(p)
      return(invisible())
    }    
  }    
})

#' @rdname plot-methods
#' @aliases plotAffinityMap,ANY-method
setMethod("plotAffinityMap", "ANY", function(x, y, 
  gases = 0, 
  main = "Affinity Map", xlab, ylab, ...)
{
  if(sum(gases == 0)) gases <- gases(x)
  nsensors <- nsensors(x)
  

  gind <- gind(x)
  gi <- gind[gases]
  gnames <- gnames(x)
  gn <- gnames[gi]
  
  if(nsensors < 2) 
    stop("Error in ANY::plotAffinityMap: 'nsensors < 2'.")
  if(length(gases) != 2) {
    stop("Error in ANY::plotAffinityMap: 'length(gases) != 2'.")
  }
  
  aff <- affinity(x, gases=gases)
    
  if(!require(MASS)) {
    stop("Package `MASS` is needed for plotting.")
  } 
  d <- MASS::kde2d(aff[1, ], aff[2, ], n=50)

  col <- mapcol(x)
  if(missing(xlab)) xlab <- paste("Affinity of Gas", gnames[1])
  if(missing(ylab)) ylab <- paste("Affinity of Gas", gnames[2])  

  image(d, col=col,
    main=main, xlab=xlab, ylab=ylab, ...)
  contour(d, add=TRUE, nlevels=10)
})

#' @rdname plot-methods
#' @aliases plotAffinity,ANY-method
setMethod("plotAffinity", "ANY", function(x, y, 
  gases = 0, 
  main = "Affinity Map", xlab, ylab, ...)
{
  if(sum(gases == 0)) gases <- gases(x)
  nsensors <- nsensors(x)
  

  gind <- gind(x)
  gi <- gind[gases]
  gnames <- gnames(x)
  gn <- gnames[gi]
  
  if(nsensors < 2) 
    stop("Error in ANY::plotAffinity: 'nsensors < 2'.")
  if(length(gases) != 2) {
    stop("Error in ANY::plotAffinity: 'length(gases) != 2'.")
  }
  
  aff <- affinity(x, gases=gases)
    
  if(!require(MASS)) {
    stop("Package `MASS` is needed for plotting.")
  } 
  d <- MASS::kde2d(aff[1, ], aff[2, ], n=50)

  col <- mapcol(x)
  if(missing(xlab)) xlab <- paste("Affinity of Gas", gnames[1])
  if(missing(ylab)) ylab <- paste("Affinity of Gas", gnames[2])  

  image(d, col=col,
    main=main, xlab=xlab, ylab=ylab, ...)
  contour(d, add=TRUE, nlevels=10)
})

#' @rdname plot-methods
#' @aliases plotMixture,ANY-method
setMethod("plotMixture", "ANY", function(x, y, n = 20, concUnits = "default",
  gases = 0, las2 = 1,
  axes = TRUE, col, nlevels = 10,
  main = "Response to Binary Mixture", xlab, ylab, ...)
{
  if(sum(gases == 0)) {
    gases <- gases(x)
    if(length(gases) > 2) gases <- gases[1:2]
  }
  nsensors <- nsensors(x)

  if(nsensors != 1) 
    stop("Error in ANY::plotMixture: 'nsensors != 1'.")
  if(length(gases) != 2)
    stop("Error in ANY::plotMixture: 'length(gind) != 2'.")

  if(concUnits == "default") concUnits <- concUnits(x)

  gind <- gind(x)
  gi <- gind[gases]
  gnames <- gnames(x)
  gn <- gnames[gi]
  
  nsd(x) <- 0

  conc <- concSample(x, "mixture", gases=gases, n=n, concUnits=concUnits)
  sdata <- predict(x, conc=conc, concUnits=concUnits)

  ### plot parameters
  if(missing(col)) col <- mapcol(x)
  if(missing(xlab)) xlab <- paste("Gas ", gn[1], ", ", ConcUnitsStr(concUnits), sep="")
  if(missing(ylab)) ylab <- paste("Gas ", gn[2], ", ", ConcUnitsStr(concUnits), sep="")
  if(missing(main)) main <- paste(main, "\n num", num(x))
  img <- matrix(as.numeric(sdata), n, n)

  image(img, col=col, axes=FALSE,
    xlab=xlab, ylab=ylab, main=main, ...)
  contour(img, add=TRUE, nlevels=nlevels)
  
  if(axes) {
    ind <- c(1, as.integer(n/2), n)
    xt <- round(conc[1:n, gi[1]], 2)
    yt <- round(conc[seq(1, n*n, by=n), gi[2]], 2)
  
    axis(1, at=seq(0, 1, length.out=n)[ind], labels=xt[ind], tick=TRUE)  
    axis(2, at=seq(0, 1, length.out=n)[ind], labels=yt[ind], las=las2, tick=TRUE) 
  }
})

#' @rdname plot-methods
#' @aliases plotSignal,ANY-method
setMethod("plotSignal", "ANY", function(x, y, conc, set,
  concUnits = "default", 
  main = "Model Response to Concentration", 
  ret = TRUE, ...)
{
  
  if(concUnits == "default") concUnits <- defaultConcUnits()
        
  gind <- gind(x)
  nsensors <- nsensors(x)
  ngases <- ngases(x)
  gnames <- gnames(x)
  
  tunit <- tunit(x)
  
  # conc
  if(missing(conc)) {
    sc <- Scenario(T = set, tunit = tunit, concUnits = concUnits)
    conc <- getConc(sc)
  }
  
  # sdata
  sdata <- predict(x, conc, concUnits = concUnits)
  
  # plot
  cf <- melt(conc, varnames = c("sample", "data")) # Concentration Frame
  sf <- melt(sdata, varnames = c("sample", "data")) # Sensor data Frame
  
  df <- rbind(data.frame(cf, matrix = "conc"), data.frame(sf, matrix = "sdata"))
  
  p <- ggplot(df, aes(x = sample, y = value)) + geom_line(aes(color = as.factor(data))) +
    facet_grid(matrix ~ ., scales = "free_y") + 
    labs(title = main)
  
  if(ret) {
    return(p)
  } 
  else {
    print(p)
    return(invisible())
  } 
})
#----------------------------
# Color Methods
#----------------------------
ccol.function <- function(conc, pal,
  gases, ngases, ...)
{
  nsamples <- nrow(conc)

  # cmode
  all.pure <- (sum(apply(conc, 1, function(x) sum(x!=0) <= 1)) == nsamples)
  cmode <- ifelse(all.pure, 'pures', 'mixtures')
  
  col <- rep("black", nsamples)
  #if(cmode == 'pures') {
    for(i in 1:ngases) {
      g <- gases[i]
      ind <- which(apply(conc, 1, function(x) sum(x[-i]) == 0)) # pure gas i
      conci <- conc[, i]
      
      pali <- pal[[i]]
      col[ind] <- pali[4]
    }
  #}
  #else {
  #  warning("Error in 'ANY::ccol': 'cmode' is unknown.")        
  #}

  return(col)  
}
  
#' @rdname plot-methods
#' @aliases ccol,ANY-method
setMethod("ccol", "ANY", function(object, conc, pal, ...)
{ 
  # missing object/conc
  missing.object <- ifelse(missing(object), TRUE, FALSE) 
  missing.conc <- ifelse(missing(conc), TRUE, FALSE) 

  if(!missing.object) {
    if(class(object) %in% c("matrix", "data.frame")) {
      missing.object <- TRUE
      missing.conc <- FALSE
      conc <- object
    }
  }
  
  if(missing.conc)
    stop("Error in 'ANY::ccol': 'conc' is missing.")        
      
  palA <- brewer.pal(4, "Blues")
  palB <- brewer.pal(4, "Reds")
  palC <- brewer.pal(4, "Greens")
  
  pal <- list(A=palA, B=palB, C=palC)

  gases <- ifelse(missing.object, gases(), gases(object))
  ngases <- ifelse(missing.object, ngases(), ngases(object))  
  
  col <- ccol.function(conc, pal, 
    gases=gases, ngases=ngases)

  return(col)
})

#' @rdname plot-methods
#' @aliases gcol,ANY-method
setMethod("gcol", "ANY", function(object, gases=0, pal, ...)
{
  if(sum(gases == 0)) gases <- gases(object)
  # gases: color
  # A: 1, B: 2, C: 3 
  # AB: 4, AC: 5, BC: 6
  # ABC: 7
  npal <- 7  
  if(missing(pal)) pal <- brewer.pal(npal, "Set1") 
  
  col <- pal[gases]
  
  return(col)
})

#' @rdname plot-methods
#' @aliases mapcol,ANY-method
setMethod("mapcol", "ANY", function(object, col, n=100, ...)
{
  if(missing(col)) col <- rev(c("#D73027", "#FC8D59", "#FEE090", "#FFFFBF", "#E0F3F8", "#91BFDB", "#4575B4")) # base colors
  
  pal <- colorRampPalette(col)(n)
  
  return(pal)
})

#' @rdname plot-methods
#' @aliases scol,ANY-method
setMethod("scol", "ANY", function(object, n, 
  start = 0.2, end = 0.8, gamma = 2.2)
{
  if(missing(n)) 
    stop("Error in ANY::scol: 'n' is missing.")

  pal <- gray.colors(n=n, start = start, end = end, gamma = gamma)
  
  return(pal)
})

#----------------------------
# Other Methods
#----------------------------

#' @rdname SensorArray-class
#' @aliases affinity,ANY-method
setMethod("affinity", "ANY", function(object, concUnits = "norm", 
  gases = 0, type = "inc", n = 300, ...)
{
  if(sum(gases == 0)) gases <- gases(object)

  gind <- gind(object)
  ngases <- ngases(object)
  ng <- length(gases)
  nsensors <- nsensors(object)
  
  nsd(object) <- 0
  
  # compute 'adata'
  adata <- array(NA, c(n, ng, nsensors))
  cdata <- array(NA, c(n, ng, nsensors)) 
  for(s in 1:nsensors) {
    for(g in gases) {
      gi <- gind[g]
      
      conc <- concSample(object, gases=g, type=type, concUnits=concUnits, n=n)
      sdata <- predict(object, conc, concUnits=concUnits)
      
      adata[, gi, ] <- sdata
      cdata[, gi, ] <- conc[, gi]
    }
  }
  
  # compute 'Kd'
  Ka <- array(0, c(ng, nsensors))
  for(s in 1:nsensors) {
    adata.min <- min(as.numeric(adata[, , s]))
    adata.max <- max(as.numeric(adata[, , s]))
    adata.mid <- 0.5 * (adata.min + adata.max)
    
    for(g in gases) {
      gi <- gind[g]
      
      sdata <- adata[, gi, s]
      if(max(sdata) > adata.mid) {
        ind <- which.min(abs(sdata - adata.mid))
        if(length(ind) != 1)
          stop("Error in ANY::affinity: 'ngth(ind) != 1'.")

        Ka[gi, s] <- 1 / cdata[ind, gi, s]
      }  
    }
  } 

  return(Ka)
})

