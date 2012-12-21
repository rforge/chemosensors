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
  par <- list(name = "undefined", gases=1:3, gnames=LETTERS[1:3], 
    concUnits=defaultConcUnits(), concUnitsInt=defaultConcUnits(),
    tunit = 1)
  
  return(par)
}

### Constructor of Scenario class.
setMethod("initialize", "Scenario", function(.Object,
  name = "character",
  # common for sub-classes
  gases="numeric", gnames="character", concUnits="character", concUnitsInt="character",
  # specific for class Scenario
  tunit = "numeric", 
  T = "character", nT = "numeric", V = "character", nV = "numeric", ...)
{   
  # missing
  def.par <- defaultParScenario()
  
  if(missing(name)) name <- def.par$name  
  if(missing(gases)) gases <- def.par$gases
  if(missing(gnames)) gnames <- def.par$gnames
  if(missing(concUnits)) concUnits <- def.par$concUnits
  if(missing(concUnitsInt)) concUnitsInt <- def.par$concUnitsInt

  if(missing(tunit)) tunit <- def.par$tunit
         
  if(missing(T)) T <- character(0)
  if(missing(nT)) nT <- rep(1, length(T))
  if(missing(V)) V <- character(0)
  if(missing(nV)) nV <- rep(1, length(V))

  if(length(nT) == 1) nT <- rep(nT, length(T))
  stopifnot(length(nT) == length(T))
  
  if(length(nV) == 1) nV <- rep(nV, length(V))  
  stopifnot(length(nV) == length(V))

  # filter by 'gases'
  ngases <- length(gases)
  if(length(gnames) != ngases) gnames <- gnames[gases]
  gind <- rep(NA, max(gases))
  for(i in 1:length(gases)) {
    g <- gases[i]
    gind[g] <- i
  }

  # assign
  .Object@name <- name
  .Object@gases <- gases
  .Object@gind <- gind  
  .Object@ngases <- ngases
  .Object@gnames <- gnames
  .Object@concUnits <- concUnits
  .Object@concUnitsInt <- concUnitsInt

  .Object@T <- T
  .Object@V <- V
  .Object@nT <- nT
  .Object@nV <- nV
  
  .Object@tunit <- tunit

  .Object@df <- data.frame()
  
  validObject(.Object)
  return(.Object)
})

#' @export
Scenario <- function(...)
{
  new("Scenario", ...)
}

#----------------------------
# Export Methods
#----------------------------
setMethod("sdata.frame", "Scenario", function(x, unique = FALSE, step = FALSE, ...) 
{ 
  lab <- set2lab(x, unique = unique)
  df <- lab2df(x, lab, step = step)
  
  return(df)
})

#----------------------------
# Conc Methods
#----------------------------


#----------------------------
# Get/Set Methods
#----------------------------

setReplaceMethod("add", "Scenario", function(object, value) 
{
  stop("not implemented yet")
    
  validObject(object)
  return(object)
})

setMethod("set2lab", "Scenario", function(object, unique = FALSE, ...)
{
  T <- object@T
  nT <- object@nT
  V <- object@V
  nV <- object@nV

  stopifnot(length(nT) == length(T))
  stopifnot(length(nV) == length(V))
  
  if(unique) {
    ind <- which(!duplicated(T))
    T <- T[ind]
    nT <- rep(1, length(ind))

    ind <- which(!duplicated(V))
    V <- V[ind]
    nV <- rep(1, length(ind))
  }
 
  df <- data.frame(set = "T", label = T, n = nT)
  if(length(nV)) df <- rbind(df, data.frame(set = "V", label = V, n = nV))
  df$index <- 1:nrow(df)
  
  label <- as.character(df$label)
  label <- lapply(strsplit(label, ","), function(x) { 
      x <- strsplit(x, " ")
      lapply(x, function(x) x[nchar(x) > 0])
  })
  
  lf <- ldply(1:length(label), 
    function(i) data.frame(index = i, 
      gas = unlist(lapply(label[[i]], function(x) x[1])),
      conc = unlist(lapply(label[[i]], function(x) as.numeric(ifelse(length(x) > 1, x[2], NA))))))
  
  df <- join(lf, df, by = "index")  
  
  return(df)
})


setMethod("lab2df", "Scenario", function(object, lab, step = FALSE, ...)
{
  cmatrix <- matrix(nrow = 0, ncol = object@ngases)
  colnames(cmatrix) <- object@gnames
  
  # output frame
  of <- data.frame(as.data.frame(cmatrix), stringsAsFactors = FALSE)
  of <- cbind(of, data.frame(time = numeric(0), set = character(0), 
    lab = character(0), tpoint = character(0), stringsAsFactors = FALSE))

  #of <- d_ply(lab, "index", function(df) {
  #  value <- list(gas = as.character(df$gas), conc = df$conc, n = unique(df$n), set = as.character(unique(df$set)))
  #  print(value) 
  #})
  
  of <- ddply(lab, "index", function(df) {
    value <- list(gas = as.character(df$gas), conc = df$conc, n = unique(df$n), set = as.character(unique(df$set)))
    label2df(object, value)#, step = step)
  })
  
  if(step) {
    of$time <- seq(1, by = object@tunit, length = nrow(of))
  }
  else {
    of$time <- 1:nrow(of)  
  }
  
  return(of)
})

setMethod("label2df", "Scenario", function(object, value, nsamples = 0, step = FALSE, ...)
{
  if(class(value) != "list")  
    stop("Error in Scenario::label2df: value must be a list (gas, conc. level, #samples).")
  if(length(value) < 3)
    stop("Error in Scenario::label2df: value must be a list of 3 entries.")
  if(length(value) == 3)  # if 3 parameters, the 4th will be added (Training Set)
    value <- c(value, "T")

  concUnitsInt <- concUnitsInt(object)
  gases <- gases(object)
  gnames <- gnames(object)
  gind <- gind(object)
  ngases <- ngases(object)
 
  tunit <- ifelse(step, 1, tunit(object))

  # parameter #1 (gas)
  par.gas <- value[[1]]
  gi <- switch(class(par.gas),
    character = as.numeric(sapply(par.gas, function(x) which(x == gnames))), 
    numeric = par.gas,
    integer = par.gas,    
    stop("Error in Scenario::label2df: first entry (gas) must be a character or a number."))
  if(!length(gi)) 
    stop("Error in Scenario::label2df: first entry (gas) is incorrect (wrong gas number or gas character?).")
  if(sum(is.na(gi)))
    stop("Error in Scenario::label2df: first entry (gas) is incorrect (wrong gas character?).")    
  if(sum(!(gi %in% gind)))
    stop("Error in Scenario::label2df: first entry (gas) is incorrect (wrong gas number or gas character?).")    

  gname <- gnames[gi] 
  cval.max <- concMax(object)[gi]
  
  # parameter #2 (conc)
  cval <- value[[2]]
  ind <- which(is.na(cval))
  if(length(ind)) {
    cval[ind] <- cval.max[ind]
  }
  if(!(class(cval) %in% c("numeric", "integer")))
    stop("Error in Scenario::label2df: second entry (conc. level) must be a numeric.")
  if(sum(cval < 0))
    stop("Error in Scenario::label2df: second entry (conc. level) must be a non-negative value.")
  if(length(cval) != length(gi))
    stop("Error in Scenario::label2df: second entry (conc. level) must have the same length as the first entry (gas).")  

  # parameter #3 (n)
  n <- value[[3]]
  if(!(class(n) %in% c("integer", "numeric")))
    stop("Error in Scenario::label2df: third entry (#samples) must be a numeric.")
  n <- as.integer(n)
  if(n <= 0)
    stop("Error in Scenario::label2df: third entry (#samples) must be a positive integer.")

  # parameter #4 (set)
  set <- value[[4]]
  if(class(set) != "character")
    stop("Error in Scenario::label2df: forth entry (set) must be a character.")  
  if(length(set) > 2)
    stop("Error in Scenario::label2df: forth entry (set) must be a vector of maximum 2 elements.")  

  if(!(sum(set %in% c("T", "V"))))
    stop("Error in Scenario::label2df: forth entry (set) is unknown; must be 'T', 'V' or 'I'.")  

  # check par.
  if(sum(cval > 2.0 * cval.max)) 
    stop("Error in Scenario::label2df: second entry (conc. level) ", paste(cval, collapse=", "), 
    " is greater than the 2 maximum levels ", paste(2.0 * cval.max, collapse=", "), "", " for gas ", paste(gname, collapse=", "), " at conc. units '", 
    concUnitsInt, "'.", sep="")
      
  # define 'conc'
  conc.row <- rep(0, ngases)
  conc.row[gi] <- cval
  
  conc0 <- matrix(0, nrow = tunit, ncol = ngases)
  conci <- matrix(conc.row, nrow = tunit, ncol = ngases, byrow=TRUE) 
  
  conci <- rbind(conci, conc0) # gas/air or response/cleaning phases
  df <- conc2df(object, conci) # columns: gas names, glab, lab, tpoint
  
  # replicate by 'n'
  df <- df[rep(1:nrow(df), n), ]

  # add column 'time'
  time.last <- nsamples # ifelse(nsamples, object@df$time[nsamples], 0)
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
  
  return(df)
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

#----------------------------
# Plot Methods
#----------------------------
#' @export
plotScenario <- function(...) plot.Scenario.class(...)

setMethod("plot", "Scenario", function (x, y, ret = FALSE, ...) 
{
  # missing
  if(missing(y)) y <- "class"
  
  p <- switch(y,
    class = plot.Scenario.time(x, y, ...),
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

plot.Scenario.class <- function(x, y, concUnits = "character", 
  points = FALSE,
  col, lty, lwd = 2,
  main = paste("Scenario: Concentration Timeline"), 
  xlab = "Time, a.u.", ylab="Concentration", ...)
{
  # process 'x'
  flag.list <- class(x) == "list"
  if(flag.list) {
    classes <- laply(x, function(x) as.character(class(x)))
    stopifnot(all(classes == "Scenario"))
    
    names <- laply(x, function(x) x@name)
    stopifnot(all(!duplicated(names)))
    
    xx <- x[-1]
    x <- x[[1]]
  }
  if(missing(concUnits)) concUnits <- concUnits(x) # 'norm'

  ngases <- ngases(x)
  gnames <- gnames(x)

  if(missing(col)) col <- gcol(x)
  ylab <- paste(ylab, ", ", ConcUnitsStr(concUnits), sep = "")

  df <- sdata.frame(x, unique = TRUE, step = FALSE)
  mf <- melt(df, measure.vars = gnames)
  mf <- data.frame(mf, scenario = x@name)

  if(flag.list) {
    for(i in 1:length(xx)) {
      xi <- xx[[i]]

      dfi <- sdata.frame(xi, unique = TRUE)
      mfi <- melt(dfi, measure.vars = gnames)
      mfi <- data.frame(mfi, scenario = xi@name)

      mf <- rbind(mf, mfi)        
    }    
  }
  
  mf <- mutate(mf,
    set = ifelse(set == "T", "Training Set", ifelse(set == "V", "Validation Set", "Unknown Set")),
    gas = variable)

  p <- ggplot(mf, aes(x = time, y = value)) + 
    geom_area(aes(color = gas, fill = gas), position = "stack") + 
    scale_fill_manual(values = col) + 
    scale_colour_manual(values = rep("white", length(col))) + 
    facet_grid(scenario ~ set) +
    labs(x = xlab, y = ylab) 
  
  return(p)    
}  

plot.Scenario.time <- function(x, y, concUnits = "character", 
  points = FALSE,
  col, lty, lwd = 2,
  main = paste("Scenario: Concentration Timeline"), 
  xlab = "Time, a.u.", ylab="Concentration", 
  graphics = "ggplot", facet = TRUE, ...)
{
  # check par. 'graphics'
  match.arg(graphics, c("base", "ggplot"))

  # process 'x'
  flag.list <- class(x) == "list"
  if(flag.list) {
    classes <- laply(x, function(x) as.character(class(x)))
    stopifnot(all(classes == "Scenario"))
    
    names <- laply(x, function(x) x@name)
    stopifnot(all(!duplicated(names)))
    
    xx <- x[-1]
    x <- x[[1]]
  }
  if(missing(concUnits)) concUnits <- concUnits(x) # 'norm'

  ngases <- ngases(x)
  gnames <- gnames(x)

  if(missing(col)) col <- gcol(x)
  ylab <- paste(ylab, ", ", ConcUnitsStr(concUnits), sep = "")

  df <- sdata.frame(x, unique = TRUE)
  mf <- melt(df, measure.vars = gnames)
  mf <- data.frame(mf, scenario = x@name)

  if(flag.list) {
    for(i in 1:length(xx)) {
      xi <- xx[[i]]

      dfi <- sdata.frame(xi, unique = TRUE)
      mfi <- melt(dfi, measure.vars = gnames)
      mfi <- data.frame(mfi, scenario = xi@name)

      mf <- rbind(mf, mfi)        
    }    
  }
  
  mf <- mutate(mf,
    set = ifelse(set == "T", "Training Set", ifelse(set == "V", "Validation Set", "Unknown Set")),
    gas = variable)

  p <- ggplot(mf, aes(x = time, y = value)) + 
    geom_area(aes(color = gas, fill = gas), position = "stack") + 
    scale_fill_manual(values = col) + 
    scale_colour_manual(values = rep("white", length(col))) + 
    facet_grid(scenario ~ set) +
    labs(x = xlab, y = ylab) 
  
  return(p)      
  
  ############
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

