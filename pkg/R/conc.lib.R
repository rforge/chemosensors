
#' @include ChemosensorsClass.R
NULL

#----------------------------
# Extract Methods
#----------------------------
setMethod("extractConc", "ANY", function(object, conc, set, scenario, n,
  cf, df, 
  concUnits = "default", ...)
{
  if(concUnits == "default") concUnits <- defaultConcUnits()
  if(missing(n)) n <- 1
  
  tunit <- tunit(object)
  
  if(!missing(conc)) { # conc
    conc <- conc
  }
  else if(!missing(cf)) { # cf
    stopifnot(all(gnames(object) %in% names(cf)))
    conc <- subset(cf, select = gnames(object), drop = FALSE)
    conc <- as.matrix(conc)
  }
  else if(!missing(df)) { # df
    stopifnot(all(gnames(object) %in% names(df)))
    conc <- subset(df, select = gnames(object), drop = FALSE)
    conc <- as.matrix(conc)
  }
  else if(!missing(set)) { # set
    sc <- Scenario(T = set, nT = n, tunit = tunit, concUnits = concUnits)
    conc <- getConc(sc)
  }  
  else if(!missing(scenario)) {
    conc <- getConc(scenario)
  }
  else {  # all parameters are missing => use function `defaultSet`
    set <- defaultSet()
    sc <- Scenario(T = set, nT = n, tunit = tunit, concUnits = concUnits)
    conc <- getConc(sc)
  }
  
  return(conc)
})

#----------------------------
# Check Methods
#----------------------------

### Method checkConc
# Function checks if 'conc' can be devided into pulses

#' @rdname scenario-methods
#' @aliases checkConc,ANY-method
setMethod("checkConc", "ANY", function(object, conc, ...)
{
  n <- nrow(conc)
  tunit <- tunit(object)
  
  # minimal length
  length.ok <- (n >= tunit * 2)
  if(!length.ok) 
    warning("Warning in checkConc: concentration matrix 'conc' is incorrect:\n",
      " - (minimal length) #samples must be at least equal to  '2 * tunits' (", 2 * tunit, ").", sep = "")
  conc.ok <- length.ok

  # multiples
  multiple.ok <- (n %% (2 * tunit)) == 0
  if(!multiple.ok) 
    warning("Warning in checkConc: concentration matrix 'conc' is incorrect:\n",
      " - (multiples) #samples must be multiple of '2 * tunits' (", 2 * tunit, ").", sep = "")
  conc.ok <- conc.ok & multiple.ok
  
  # pulses' length
  pulse.ok <- FALSE
  if(conc.ok) {
    airin <- getTPoint(object, conc, "airin")
    airout <- getTPoint(object, conc, "airout")
    gasin <- getTPoint(object, conc, "gasin")
    gasout <- getTPoint(object, conc, "gasout")

    np <- length(gasin)
    pulse.ok <- (length(gasout) == np) & (length(airin) == np) & (length(airout) == np)
  }
  if(!pulse.ok) 
    warning("Warning in checkConc: concentration matrix 'conc' is incorrect:\n",
      " * (pulses' length) pulse must be composed of two parts (gas and air).\n", sep = "")
  conc.ok <- conc.ok & pulse.ok
  
  # labels
  label.ok <- FALSE
  if(conc.ok) {
    conc.df <- conc2df(object, conc)
    # gas phase of the pulse is either (1) gas or (2) air
    gas.ind <- data.frame(start = gasin, end = gasout)
    
    tunit.one <- with(gas.ind, all(start == end))
    gas.ok <- ifelse(tunit.one,
      all(apply(gas.ind, 1, function(x) conc.df$tpoint[x[1]] == "gasin")),
      all(apply(gas.ind, 1, function(x) 
        all(conc.df$tpoint[seq(x[1]+1, x[2]-1)] == "gas") | 
        all(conc.df$tpoint[seq(x[1]+1, x[2]-1)] == "air"))))
        
    # air phase of the pulse is air
    air.ind <- data.frame(start = airin, end = airout)
    tunit.one <- with(air.ind, all(start == end))
    air.ok <- ifelse(tunit.one,
      all(apply(air.ind, 1, function(x) conc.df$tpoint[x[1]] == "airin")),      
      all(apply(air.ind, 1, function(x) all(conc.df$tpoint[seq(x[1]+1, x[2]-1)] == "air"))))
      
    label.ok <- gas.ok & air.ok
  }
  if(!label.ok)
    warning("Warning in checkConc: concentration matrix 'conc' is incorrect:\n",
      " * (labels) pulse must be composed of two parts (gas and air).\n", sep = "")
  conc.ok <- conc.ok & label.ok
  
  return(conc.ok)
})

#----------------------------
# Conc Methods
#----------------------------

#' Method conc2df
#'
#' Method conc2df converts a concetration matrix into a data frame.
#'
#' The input parameters are an object, e.g. \code{SensorArray}, and a concentration matrix.
#' The output data frame has the following columns:
#'
#' \tabular{rl}{
#'   \code{A}, \code{B}, ... \tab Gas concentrations (column names equal to gas names of the object). \cr
#'   \code{glab} \tab Gas labels, e.g. \code{A} or \code{Air}. \cr
#'   \code{lab} \tab Gas+Concetratoin labels, e.g. \code{A 0.01}. \cr
#'   \code{tpoint} \tab Time point labels to encode the gas pulses, e.g. \code{gasin}.
#' }
#' 
#' @example inst/examples/conc2df-method.R
#' @rdname scenario-methods
#' @aliases conc2df,ANY-method
setMethod("conc2df", "ANY", function(object, conc, ...)
{  
  if(missing(conc))
    stop("Error in ANY::conc2df: 'conc' is missing.")
    
  ngases <- ngases(object)
  ngases <- ifelse(!is.na(ngases), ngases, ncol(conc))
  stopifnot(ngases == ncol(conc)) 
  
  gases <- gases(object)
  gnames <- gnames(object)
  gind <- gind(object)
  
  if(ngases != ncol(conc))
    stop("Error in ANY::conc2df: 'ngases != ncol(conc)'.")  
  
  n <- nrow(conc)

  df <- data.frame(C=conc)
  colnames(df) <- c(gnames)
  df <- cbind(sample=1:n, df)

  df$glab <- apply(conc, 1, function(x, gnames) paste(paste(gnames[which(x != 0)], sep=''), collapse=", "), gnames)
  df[df$glab == "", "glab"] <- "Air"
  
  df$lab <- apply(conc, 1, function(x, gnames) paste(paste(gnames[which(x != 0)], x[x != 0], sep=''), collapse=", "), gnames)
  df[df$lab == "", "lab"] <- "Air"
    
  df$tpoint <- conc2tpoint(object, conc)
  
  return(df[, c(gnames, "glab", "lab", "tpoint")])  
})

#' @rdname scenario-methods
#' @aliases conc2lab,ANY-method
setMethod("conc2lab", "ANY", function(object, ...)
{  
  df <- conc2df(object, ...)
  lab <- df$lab
  
  return(lab)
})

#' @rdname scenario-methods
#' @aliases conc2glab,ANY-method
setMethod("conc2glab", "ANY", function(object, ...)
{  
  df <- conc2df(object, ...)
  glab <- df$glab
  
  return(glab)
})

#----------------------------
# Conc2Col Methods
#----------------------------

#' @rdname scenario-methods
#' @aliases conc2col,ANY-method
setMethod("conc2col", "ANY", function(object, conc, pal, ...)
{  
  if(missing(conc))
    stop("Error in ANY::conc2col: 'conc' is missing.")
    
  gases <- gases(object)
  gnames <- gnames(object)
  ngases <- ngases(object) 
  gind <- gind(object)

  if(ngases != ncol(conc))
    stop("Error in ANY::conc2col: 'ngases != ncol(conc)'.")  

  n <- nrow(conc)

  if(missing(pal)) {
    palA <- brewer.pal(4, "Blues")[-1]
    palB <- brewer.pal(4, "Reds")[-1]
    palC <- brewer.pal(4, "Greens")[-1]
    palAC <- brewer.pal(9, "YlGnBu")[4:8]
    palAB <- brewer.pal(9, "PuRd")[4:6]    
    palBC <- brewer.pal(9, "YlOrRd")[4:6]
    pal <- list(A=palA, B=palB, C=palC, AB=palAB, BC=palBC, AC=palAC)
  }

  df <- conc2df(object, conc, ...)  
  conc.norm <- concNorm(object, conc, concUnitsInt="norm", ...)
  
  col <- rep("black", n)  
  # pure gases
  for(i in 1:ngases) {
    g <- gases[i]
    gi <- gind[i]
    gname <- gnames[i]
    
    ind <- which(df$glab == gname)
    if(length(ind)) {
      conci <- conc.norm[ind, gi]
      conci[conci > 1] <- 1
      
      pali <- pal[[gi]]
      coli <- colorRampPalette(pali)(100)
      colind <- as.integer(100 * conci) + 1
      colind[colind > 100] <- 100

      col[ind] <- coli[colind]
    }
  }
  # binary mixtures
  for(binmix in c("AB", "BC", "AC")) { 
    bgases <- switch(binmix, 
      AB = 1:2, BC = 2:3, AC = c(1, 3))
    bpal <- switch(binmix, 
      AB = pal[[4]], BC = pal[[5]], AC = pal[[6]])
    if(sum(gases %in% bgases) == 2) {
      i <- which(gases %in% bgases)
      g <- gases[i]
      gi <- gind[i]
      gname <- gnames[i] # A, C
     
      glabi <- paste(gname, collapse=", ")
      ind <- which(df$glab == glabi)
      if(length(ind)) {
        conci <- conc.norm[ind, gi]  # 2-column matrix
        if(is.null(nrow(conci))) conci <- matrix(conci, ncol = 2)
        
        conci <- apply(conci, 1, sum)
        conci[conci > 2] <- 2
        conci <- conci / 2
      
        coli <- colorRampPalette(bpal)(100)
        colind <- as.integer(100 * conci) + 1
        colind[colind > 100] <- 100

        col[ind] <- coli[colind]
      }
    }
  }
  
  return(col)    
})


#----------------------------
# TPoint Methods
#----------------------------

### Method conc2tpoint
# "airin"  "air"    "airout" "gasin"  "gas"    "gasout"

#' @rdname scenario-methods
#' @aliases conc2tpoint,ANY-method
setMethod("conc2tpoint", "ANY", function(object, conc, ...)
{
  if(missing(conc))
    stop("Error in ANY::conc2tpoint: 'conc' is missing.")

  conc <- as.matrix(conc)
  ngases <- ngases(object)
  n <- nrow(conc)
  
  if(ncol(conc) != ngases)
    stop("Error in ANY::conc2tpoint: #columns in 'conc' is not equal to 'ngases'.")
    
  # filter into two groups: 'air' and 'gas'
  tpoint <- ifelse(apply(conc, 1, sum) == 0, "air", "gas")    
  # 'airin'
  ind.first <- (tpoint[1] == 'air')
  ind <- NULL
  if(n > 1) ind <- (tpoint[2:n] == 'air' & tpoint[1:(n-1)] != 'air')
  tpoint[c(ind.first, ind)] <- 'airin'
  # 'airout'
  ind.first <- FALSE
  ind <- NULL
  if(n > 2) ind <- tpoint[2:(n-1)] == 'air' & tpoint[3:n] != 'air'
  ind.last <- (tpoint[n] == 'air')
  tpoint[c(ind.first, ind, ind.last)] <- 'airout'  
  # 'gasin'
  ind.first <- (tpoint[1] == 'gas')
  ind <- NULL
  if(n > 1) ind <- (tpoint[2:n] == 'gas' & tpoint[1:(n-1)] != 'gas')
  tpoint[c(ind.first, ind)] <- 'gasin'
  # 'gasout'
  ind.first <- FALSE
  ind <- NULL
  if(n > 2) ind <- (tpoint[2:(n-1)] == 'gas' & tpoint[3:n] != 'gas')
  ind.last <- (tpoint[n] == 'gas')
  tpoint[c(ind.first, ind, ind.last)] <- 'gasout'  
    
  return(as.character(tpoint))
})

#' @rdname scenario-methods
#' @aliases getTPoint,ANY-method
setMethod("getTPoint", "ANY", function(object, conc, tpoint, ...)
{
  stopifnot(!missing(conc))
  stopifnot(!missing(tpoint))  
  
  tunit <- tunit(object)
  n <- nrow(conc)
  
  out <- switch(tpoint,
    "airin" = seq(1, n, by = 2 * tunit),
    "airout" = seq(tunit, n, by = 2 * tunit),
    "gasin" = seq(tunit + 1, n, by = 2 * tunit),
    "gasout" = seq(2 * tunit, n, by = 2 * tunit),
    stop("Error in ANY::getTPoint: switch."))

  return(out)
})







