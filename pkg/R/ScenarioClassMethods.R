# last revision: 12 Jan 2012

#' @include ScenarioClass.R
NULL

#----------------------------
# Class constructor
#----------------------------

#' Get default constructor parameters of class \code{\link{Scenario}}.
#' @name defaultParScenario
#' @rdname pub-defaultParScenario
#' @keywords Scenario defaults
#' @return List of the default parameters.
#' @export
defaultParScenario <- function()
{
  par <- list(gases=1:3, gnames=LETTERS[1:3], concUnits=defaultConcUnits(), concUnitsInt=defaultConcUnits(),
    tunit = 1)
  
  return(par)
}

### Constructor of Scenario class.
setMethod("initialize", "Scenario", function(.Object,
  # common for sub-classes
  gases="numeric", gnames="character", concUnits="character", concUnitsInt="character",
  # specific for class Scenario
  tunit = "numeric", 
  T = "character", nT = "numeric", V = "character", nV = "numeric", ...)
{   
  # missing
  def.par <- defaultParScenario()
  
  if(missing(gases)) gases <- def.par$gases
  if(missing(gnames)) gnames <- def.par$gnames
  if(missing(concUnits)) concUnits <- def.par$concUnits
  if(missing(concUnitsInt)) concUnitsInt <- def.par$concUnitsInt

  if(missing(tunit)) tunit <- def.par$tunit
         
  if(missing(T)) T <- character(0)
  if(missing(nT)) nT <- numeric(0)
  if(missing(V)) V <- character(0)
  if(missing(nV)) nV <- numeric(0)
             
  # filter by 'gases'
  ngases <- length(gases)
  if(length(gnames) != ngases) gnames <- gnames[gases]
  gind <- rep(NA, max(gases))
  for(i in 1:length(gases)) {
    g <- gases[i]
    gind[g] <- i
  }

  # assign
  .Object@gases <- gases
  .Object@gind <- gind  
  .Object@ngases <- ngases
  .Object@gnames <- gnames
  .Object@concUnits <- concUnits
  .Object@concUnitsInt <- concUnitsInt

  .Object@tunit <- tunit

  cmatrix <- matrix(nrow=0, ncol=ngases)
  colnames(cmatrix) <- .Object@gnames
  .Object@df <- as.data.frame(cmatrix, stringsAsFactors=FALSE)
  .Object@df <- cbind(.Object@df, data.frame(time = numeric(0), 
    set = character(0), lab = character(0), 
    tpoint = character(0), stringsAsFactors=FALSE))
  
  # add samples into Training Set
  if(length(T) & length(nT)) {
    T <- lapply(strsplit(T, ","), function(x) {
      x <- unlist(strsplit(x, " "))
      x[nchar(x) > 0]
    })

    n <- length(T)
    if(length(nT) == 1) nT <- rep(nT, n)
    stopifnot(length(nT) == length(T))
    
    for(i in 1:length(T)) {
      Ti <- T[[i]]
      ni <- length(Ti)
      add(.Object) <- list(gas = Ti[seq(1, ni, 2)], conc = as.numeric(Ti[seq(2, ni, 2)]), n = nT[i], set = "T")
    }
  }   
  
  # add samples into Validation Set
  if(length(V) & length(nV)) {
    V <- lapply(strsplit(V, ","), function(x) {
      x <- unlist(strsplit(x, " "))
      x[nchar(x) > 0]
    })

    n <- length(V)
    if(length(nV) == 1) nV <- rep(nV, n)
    stopifnot(length(nV) == length(V))
    
    for(i in 1:length(V)) {
      Vi <- V[[i]]
      ni <- length(Vi)
      add(.Object) <- list(gas = Vi[seq(1, ni, 2)], conc = as.numeric(Vi[seq(2, ni, 2)]), n = nV[i], set = "V")
    }
  }   
  validObject(.Object)
  return(.Object)
})

#' @export
Scenario <- function(...)
{
  new("Scenario", ...)
}

#----------------------------
# Plot Methods
#----------------------------
setMethod("plot", "Scenario", function (x, y, ret = FALSE, ...) 
{
  # missing
  if(missing(y)) y <- "time"
  
  p <- switch(y,
    time = plot.Scenario.time(x, y, ...),
    stop("Error in Scenario::plot: plot type 'y' is unknown."))

  if(ret) {
    return(p)
  } 
  else {
    if(!is.null(p)) print(p)
    return(invisible())
  }
})

plot.Scenario.time <- function(x, y, concUnits = "character", 
  points = FALSE,
  col, lty, lwd = 2,
  main = paste("Scenario: Concentration Timeline"), 
  xlab = "Time, a.u.", ylab="Concentration", 
  graphics = "ggplot", facet = TRUE, ...)
{
  # check par. 'graphics'
  match.arg(graphics, c("base", "ggplot"))
  
  if(missing(concUnits)) concUnits <- concUnitsInt(x) # 'norm'

  ngases <- ngases(x)
  gnames <- gnames(x)
  
  conc <- cmatrix(x)
  concUnitsInt <- concUnitsInt(x)
  if(concUnitsInt != concUnits) conc <- concNorm(x, conc, concUnitsInt=concUnits, concUnits=concUnitsInt) 

  ### plot par.
  if(missing(lty)) lty <- c(1:3)[gases(x)]
  if(missing(col)) col <- gcol(x)
  ylab <- paste(ylab, ", ", ConcUnitsStr(concUnits), sep="")

  ### plot  
  if(graphics == "base") {
    matplot(x@df$time, conc, t='l', col=col, lwd = lwd, lty = lty,
      bty='n',
      main=main, xlab = xlab, ylab = ylab, ...)  
 
    # points
    if(points) {
      ind <- which(x@df$tpoint == "gasin")
      len <- length(ind)
      if(len) points(x@df[ind, "time"], rep(0, len), pch=16)
    
      ind <- which(x@df$tpoint == "airin")
      len <- length(ind)
      if(len) points(x@df[ind, "time"], rep(0, len), pch=1)
    }
    
    return(invisible())
  }
  else if(graphics == "ggplot") {
    df <- x@df  
    df[, gnames] <- conc
  
    # filter by non-zero conc.
    conc.sum <- apply(df[, gnames], 2, sum)
    gind.nonzero <- which(conc.sum > 0)
    gind.zero <- which(conc.sum == 0)    
    stopifnot(length(gind.nonzero) > 1)

    if(length(gind.zero)) df <- df[, -gind.zero]
    mf <- melt(df, measure.vars = gnames[gind.nonzero])
    colnames(mf)[which(colnames(mf) == "variable")] <- "gas"
    
    #p <- qplot(time, value, data = mf, colour = gas, geom = "line") +
    #  scale_colour_manual(values = gcol(x)) + 
    #  facet_grid(gas ~ ., scales = "free_y") + 
    #  labs(x = xlab, y = ylab.new) + 
    #  opts(title = main)

    col <- gcol(x)[gind.nonzero]
    p <- ggplot(mf, aes(x = time, y = value)) +
      geom_area(aes(colour = gas, fill = gas), position = 'stack') +
      scale_fill_manual(values = col) + 
      scale_colour_manual(values = rep("white", length(col))) + 
      labs(x = xlab, y = ylab) + 
      opts(title = main)
      
      if(facet) p <- p + facet_grid(gas ~ ., scales = "free_y")
      
    return(p)
  }
}

#----------------------------
# Get/Set Methods
#----------------------------

setReplaceMethod("add", "Scenario", function(object, value) 
{
  if(class(value) != "list")  
    stop("Error in Scenario::add<-: value must be a list (gas, conc. level, #samples).")
  if(length(value) < 3)
    stop("Error in Scenario::add<-: value must be a list of 3 entries.")
  if(length(value) == 3)  # if 3 parameters, the 4th will be added (Training Set)
    value <- c(value, "T")

  concUnitsInt <- concUnitsInt(object)
  gases <- gases(object)
  gnames <- gnames(object)
  gind <- gind(object)
  ngases <- ngases(object)
 
  tunit <- tunit(object)
  nsamples <- nsamples(object)
  
  # parameter #1 (gas)
  par.gas <- value[[1]]
  gi <- switch(class(par.gas),
    character = as.numeric(sapply(par.gas, function(x) which(x == gnames))), 
    numeric = par.gas,
    integer = par.gas,    
    stop("Error in Scenario::add<-: first entry (gas) must be a character or a number."))
  if(!length(gi)) 
    stop("Error in Scenario::add<-: first entry (gas) is incorrect (wrong gas character or gas character?).")
  if(sum(is.na(gi)))
    stop("Error in Scenario::add<-: first entry (gas) is incorrect (wrong gas character?).")    
  if(sum(!(gi %in% gind)))
    stop("Error in Scenario::add<-: first entry (gas) is incorrect (wrong gas number or gas character?).")    

  gname <- gnames[gi] 
  
  # parameter #2 (conc)
  cval <- value[[2]]
  if(!(class(cval) %in% c("numeric", "integer")))
    stop("Error in Scenario::add<-: second entry (conc. level) must be a numeric.")
  if(sum(cval < 0))
    stop("Error in Scenario::add<-: second entry (conc. level) must be a non-negative value.")
  if(length(cval) != length(gi))
    stop("Error in Scenario::add<-: second entry (conc. level) must have the same length as the first entry (gas).")  
  
  # parameter #3 (n)
  n <- value[[3]]
  if(class(n) != "numeric")
    stop("Error in Scenario::add<-: third entry (#samples) must be a numeric.")
  n <- as.integer(n)
  if(n <= 0)
    stop("Error in Scenario::add<-: second entry (conc. level) must be a positive integer.")

  # parameter #4 (set)
  set <- value[[4]]
  if(class(set) != "character")
    stop("Error in Scenario::add<-: forth entry (set) must be a character.")  
  if(length(set) > 2)
    stop("Error in Scenario::add<-: forth entry (set) must be a vector of maximum 2 elements.")  

  if(!(sum(set %in% c("T", "V"))))
    stop("Error in Scenario::add<-: forth entry (set) is unknown; must be 'T', 'V' or 'I'.")  
      
  # check par.
  cval.max <- concMax(object)[gi]
  if(sum(cval > cval.max)) 
    stop("Error in Scenario::add<-: second entry (conc. level) ", paste(cval, collapse=", "), 
    " is greater than the maximum level ", paste(cval.max, collapse=", "), "", " for gas ", paste(gname, collapse=", "), " at conc. units '", 
    concUnitsInt, "'.", sep="")
      
  # define 'conc'
  conc.row <- rep(0, ngases)
  conc.row[gi] <- cval
  
  conc0 <- matrix(0, nrow = tunit, ncol=ngases)
  conci <- matrix(conc.row, nrow = tunit, ncol=ngases, byrow=TRUE) 
  
  conci <- rbind(conci, conc0) # gas/air or response/cleaning phases
  df <- conc2df(object, conci) # columns: gas names, glab, lab, tpoint
  
  # replicate by 'n'
  df <- df[rep(1:nrow(df), n), ]

  # add column 'time'
  time.last <- ifelse(nsamples, object@df$time[nsamples], 0)
  df$time <- seq(time.last + 1, by = 1, length = nrow(df))
  
  # add column 'set'
  # (to the last order)
  if(length(set) == 1) {
    df$set <- set
  }
  else if(length(set) == 2) {
    df$set <- set[1]
    df2 <- df
    df2$set <- set[2]
    df <- rbind(df, df2)
  }
  
  # assign df to object@df
  object@df <- rbind(object@df, df)
    
  validObject(object)
  return(object)
})


#----------------------------
# Predict Methods
#----------------------------

#----------------------------
# Model Methods
#----------------------------

setMethod("concSample", "Scenario", function(object, n = 1, ...)
{
  ngases <- ngases(object)
  gnames <- gnames(object)

  conc <- matrix(rep(0, ngases), nrow=n, ncol=ngases)
  colnames(conc) <- gnames
  
  df <- as.data.frame(conc)
  df <- cbind(df, data.frame(time=0, set="T", lab="Air", tpoint="air", stringsAsFactors=FALSE))  
  
  if(sum(colnames(df) %in% colnames(object@df)) != ncol(object@df))
    stop("Error in Scenario::concSample: colnames are not the same as in slot 'df'.")  

  return(df)
})

### Method getConc
setMethod("getConc", "Scenario", function(object, ...)
{
  conc <- cmatrix(object)
  n <- nrow(conc)
  if(n == 0) 
    stop("Error in Scenario::getConc: conc. matrix is empty (used 'add' method).")
     
  #conc <- conc[sample(1:nrow(conc)), ]
  
  return(as.matrix(conc))
})
