
#' @include ChemosensorsClass.R
NULL

#----------------------------
# PlotBoxplot Method
#----------------------------

#' @rdname plot-methods
#' @aliases plotBox,ANY-method
setMethod("plotBox", "ANY", function(x, y, conc, sdata, set, scenario, 
  cf, sf, 
  sensors = 1, feature = "step", air = TRUE, 
  concUnits = "default", 
  main = "Boxplot", xlab, ylab, 
  scales = c("fixed", "free_y"), gcol = FALSE, sensor.names = "long",
  ret = TRUE, ...)
{
  scales <- match.arg(scales)
  
  if(concUnits == "default") concUnits <- defaultConcUnits()

  stopifnot(all(sensors %in% 1:nsensors(x)))
  snames <- snames(x, ...)[sensors]          
  
  #if(dsd(x)) 
  #  warning("Warning in ANY::plotBox: drift noise 'dsd' is not zero.")    
  
  nsensors <- nsensors(x)
  gnames <- gnames(x)
  
  tunit <- tunit(x)
  
  # conc
  args <- as.list(match.call())
  args <- args[-1] 
  
  names(args)[1] <- "object" # rename first argument from `x` to `object` (as defined in function `extractConc`)
  
  conc <- do.call("extractConc", args)

  # sdata
  if(missing(sdata)) sdata <- predict(x, conc, concUnits = concUnits)
  stopifnot(nrow(sdata) == nrow(conc))
  stopifnot(ncol(sdata) == nsensors)

  df <- sdata.frame(x, conc = conc, sdata = sdata, feature = feature, ...)
  
  stopifnot(all(gnames %in% names(df)))
  stopifnot(all(snames %in% names(df)))

  # snames
  snames.old <- snames(x)
  snames.new <- snames(x, sensor.names = "long")
  names(snames.new) <- snames.old
  df <- rename(df, snames.new) 
  
  snames <- snames.new
  
  mf <- melt(df, measure.var = snames)
  
  # plot parameters
  if(missing(xlab)) xlab <- "Gas Labels"
  if(missing(ylab)) ylab <- paste("Sensor Response (feature '", feature, "')", sep = "")

  p <- qplot(lab, value, color = glab, data = mf, geom = "boxplot") +
    facet_wrap(~ variable, scales = scales) + 
    labs(title = main, x = xlab, y = ylab)

  if(gcol) {
    gcol <- gcol(x)
    glab <- unique(df$glab)
    gnames <- gnames(x)
    
    ind <- laply(glab, function(x) { 
      y <- which(gnames == x)
      ifelse(length(y), y, NA)
    })
    
    if(all(!is.na(ind))) {
      p <- p + scale_color_manual(values = gcol[ind])  
    }
  }
  if(ret) {
    return(p)
  } 
  else {
    print(p)
    return(invisible())
  } 
})

#----------------------------
# PlotRespone Method
#----------------------------

#' @rdname plot-methods
#' @aliases plotResponse,ANY-method
setMethod("plotResponse", "ANY", function(x, y, idx = 1, gas,
  concUnits = "default", 
  n = 100,
  main = "Model Response", 
  ret = TRUE, ...)
{
  if(concUnits == "default") concUnits <- defaultConcUnits()
        
  gind <- gind(x)
  nsensors <- nsensors(x)
  ngases <- ngases(x)
  gnames <- gnames(x)

  if(missing(gas)) gas <- gnames
  stopifnot(all(gas %in% gnames))
  
  enableDyn(x) <- FALSE
  nsd(x) <- 0

  conc0 <- matrix(0, nrow = n, ncol = ngases)  
  concMax.values <- concMax(x, concUnits = concUnits)
  
  # conc
  df <- data.frame(check.names = FALSE)
  for(i in 1:ngases) {
    gi <- gind[i]
    gni <- gnames[i]
    ci <-concMax.values[i]
    
    if(gni %in% gas) {
      conci <- conc0
      conci[, gi] <- seq(0, ci, length = n)
    
      sdata <- predict(x, conc = conci, concUnits = concUnits)
      sdatai <- sdata[, idx, drop = FALSE]
    
      dfi <- data.frame(sample = 1:n, conc = conci[, gi], sdata = sdata[, 1], gas = gni)
      
      df <- rbind(df, dfi)
    }
  }
  
  # plot
  main <- paste(main, paste("(S", x@idx[idx], ", num ", x@num[idx], ")", sep = ""))
  
  mf <- melt(df, measure.vars = c("conc", "sdata"))

  p <- ggplot(mf, aes(sample, value)) + 
    geom_line(aes(color = variable)) + 
    facet_wrap(variable ~ gas, scales = "free") +
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
# PlotPCA Method
#----------------------------

#' @rdname plot-methods
#' @aliases plotPCA,ANY-method
setMethod("plotPCA", "ANY", function(x, y, conc, sdata, set, scenario,
  feature = "transient", air = TRUE, 
  mod, center = TRUE, scale = TRUE, pc = 1:2,
  concUnits = "default", 
  main = "PCA scoreplot", xlab, ylab, 
  size = NULL, alpha = NULL,
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
  if(missing(sdata)) sdata <- predict(x, conc, concUnits = concUnits)
  stopifnot(nrow(sdata) == nrow(conc))
  stopifnot(ncol(sdata) == nsensors)
  
  # filter off air
  if(feature == "transient" & !air) { 
    ind <- apply(conc, 1, sum) == 0
    if(length(ind)) {
      conc <- conc[!ind, , drop = FALSE]
      sdata <- sdata[!ind, , drop = FALSE]
    }
  }
  
  # feature
  if(feature != "transient") {
    df <- sdata.frame(x, conc = conc, sdata = sdata, feature = feature, ...)
    
    stopifnot(all(gnames(x) %in% names(df)))
    conc <- subset(df, select = gnames(x))
    
    stopifnot(all(snames(x) %in% names(df)))
    sdata <- subset(df, select = snames(x))
  }
  
  # model
  if(missing(mod)) mod <- prcomp(sdata, center = center, scale = scale)
  
  # scores
  X <- sdata
  if(mod$center[1]) X <- as.matrix(sweep(X, 2, mod$center))
  if(mod$scale[1])  X <- as.matrix(sweep(X, 2, mod$scale, "/"))
  
  scores <- X %*% mod$rotation[, pc]
  
  # plot parameters
  if(missing(xlab)) xlab <- paste("PC", pc[1], " (", round(100*capturedVar(sdata, pc[1], mod), 2), ")%", sep="") 
  if(missing(ylab)) ylab <- paste("PC", pc[2], " (", round(100*capturedVar(sdata, pc[2], mod), 2), ")%", sep="")   

  # plot
  cf <- conc2df(x, conc)

  df <- as.data.frame(scores)
  colnames(df) <- c("PCx", "PCy")

  df <- cbind(df, cf)
  
  p <- ggplot(df, aes(PCx, PCy)) +  
    labs(title = main, x = xlab, y = ylab)

  if(!is.null(size) && !is.null(alpha)) {
    p <- p + geom_point(aes(color = lab), size = size, alpha = alpha)  
  }
  else if(!is.null(size)) {
    p <- p + geom_point(aes(color = lab), size = size)
  }
  else {
    p <- p + geom_point(aes(color = lab))
  }
  
  if(ret) {
    return(p)
  } 
  else {
    print(p)
    return(invisible())
  } 
})

