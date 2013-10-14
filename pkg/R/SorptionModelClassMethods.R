# last revision: 16 Jan 2012

#' @include SorptionModelClass.R
NULL

#----------------------------
# Class constructor
#----------------------------

#' Function to get default constructor parameters of class \code{\link{SorptionModel}}.
#' @rdname SorptionModel-class
#' @aliases defaultSorptionModel
#' @return List of the default parameters.
#' @export
defaultSorptionModel <- function()
{
  par <- list(gases=1:3, gnames=LETTERS[1:3], concUnits="perc", concUnitsSorption=defaultConcUnitsSorption(),
    knum=1,
    datasetSorptionModel=defaultDataSorptionModel(), pck=defaultDataPackage(),
    srdata=NULL, Qequal=TRUE, Knorm=TRUE, Kmin=1, Kmax=150, alpha = 2.25)
  
  return(par)
}

#' Constructor method of SorptionModel Class.
#'
#' @name SorptionModel
#' @rdname SorptionModel-class
setMethod("initialize", "SorptionModel", function(.Object,
  # common for sub-classes
  gases="numeric", gnames="character", concUnits="character", concUnitsSorption="character",
  # specific for class SorptionModel
  datasetSorptionModel = "character", pck = "character",
  knum = "numeric", srdata, Qequal="logic", Knorm="logic", 
  Kmin="numeric", Kmax="numeric", alpha = "numeric", ...)
{   
  # missing
  def.par <- defaultSorptionModel()

  if(missing(gases)) gases <- def.par$gases
  if(missing(gnames)) gnames <- def.par$gnames
  if(missing(concUnits)) concUnits <- def.par$concUnits
  if(missing(concUnitsSorption)) concUnitsSorption <- def.par$concUnitsSorption

  if(missing(datasetSorptionModel)) datasetSorptionModel <- def.par$datasetSorptionModel
  if(missing(pck)) pck <- def.par$pck
  if(missing(srdata)) srdata <- def.par$srdata
  if(missing(Qequal)) Qequal <- def.par$Qequal
  if(missing(Knorm)) Knorm <- def.par$Knorm
  if(missing(Kmin)) Kmin <- def.par$Kmin
  if(missing(Kmax)) Kmax <- def.par$Kmax  
  if(missing(alpha)) alpha <- def.par$alpha  
  
  if(missing(knum)) knum <- def.par$knum
  idx <- 1:length(knum)

  # process 'alpha'
  Kmax <- alpha * Kmax
  if(Kmax <= Kmin)
    stop("Error in SorptionModel::initialize: 'Kmax <= Kmin'; check parameter 'alpha'.")
    
  # load data
  if(is.null(srdata)) {
    #data(list=datasetSorptionModel, package=pck, envir=environment()) # -> 'qkc'
    #if(!(exists("qkc")))
    #  stop("Error in SorptionModel::initialize: 'datasetSorptionModel' is not loaded; variable 'qkc' is not found.")    
    
    #if(!exists(datasetSorptionModel)) # datasetSorptionModel supposed to be `UNIMANsorption`
    #  stop("Error in SorptionModel::initialize: dataset", datasetSorptionModel, "is not loaded.")    
    #eval(parse(text = paste("mdat <-", datasetSorptionModel)))

    mdat <- loadUNIMANdata(datasetSorptionModel)
    
    qkc <- mdat$qkc

    srdata <- qkc
  }      
  
  # prepare data 'K' and 'Q'
  K <- t(srdata[, , "K"])
  Q <- t(srdata[, , "Q"])
  
  # normalize 'K'
  if(Knorm) {
    affinity <- K
    M <- max(affinity)
    m <- min(affinity)
    affinity <- sweep(affinity, 2, m, "-")
    affinity <- sweep(affinity, 2, (M - m), "/")
    affinity <- initSorptionK(affinity, Kmin, Kmax)
    
    K <- affinity
  }
  else {
    Kmin <- min(K)
    Kmax <- max(K)
  }
    
  # check 'knum'
  if(sum(knum <= 0 | knum > dim(srdata)[1]))
    stop("Error in SorptionModel::initialize: 'knum' is incorrect.")  
  
  # create 'K', 'Q' from 'srdata'
  # rows == gases, columns == knum
  K <- K[, knum]
  Q <- Q[, knum]
  
  # filter 'K' and 'Q' by 'knum'
  if(is.null(ncol(K))) K <- matrix(K, ncol=1) # 1-col case
  if(is.null(ncol(Q))) Q <- matrix(Q, ncol=1) # 1-col case

  # filter by 'gases'
  ngases <- length(gases)
  if(length(gnames) != ngases) gnames <- gnames[gases]
  gind <- rep(NA, max(gases))
  for(i in 1:length(gases)) {
    g <- gases[i]
    gind[g] <- i
  }

  # filter 'K' and 'Q' by 'gases'    
  K <- K[gases, ]
  Q <- Q[gases, ]
  
  if(is.null(nrow(K))) K <- matrix(K, nrow=ngases) # 1-nrow case
  if(is.null(nrow(Q))) Q <- matrix(Q, nrow=ngases) # 1-nrow case
  
  # check 'Qequal'
  if(Qequal) {  
    #Q <- matrix(1, nrow=nrow(Q), ncol=ncol(Q))
    Q <- matrix(c(1, 1, 1)[gases], nrow=nrow(Q), ncol=ncol(Q))
  }
  else  
    stop("Error in SorptionModel::initialize: 'Qequal' is FALSE.")  
    
  # set names of 'K' and 'Q'
  colnames(K) <- idx
  rownames(K) <- gnames
  colnames(Q) <- idx
  rownames(Q) <- gnames
  
  # update 'srdata'
  srdata.names <- dimnames(srdata)
  srdata <- array(srdata[knum, gases, ], c(length(knum), ngases, dim(srdata)[3]))
  srdata.names[[1]] <- srdata.names[[1]][knum]
  srdata.names[[2]] <- srdata.names[[2]][gases]
  dimnames(srdata) <- srdata.names
  
  # assign
  .Object@idx <- idx
  .Object@gases <- gases
  .Object@gind <- gind  
  .Object@ngases <- ngases
  .Object@gnames <- gnames
  .Object@concUnits <- concUnits
  .Object@concUnitsSorption <- concUnitsSorption

  .Object@knum <- knum
  .Object@alpha <- alpha  
  .Object@srdata <- srdata  
  .Object@sorptionModel <- list(K=K, Q=Q,
    Qequal=Qequal, Knorm=Knorm, Kmin=Kmin, Kmax=Kmax)
  
  validObject(.Object)
  return(.Object)
})

#' Wrapper function SorptionModel.
#'
#' @name SorptionModel
#' @rdname SorptionModel-class
#' @param ... parameters of constructor.
#' @export
SorptionModel <- function(...)
{
  new("SorptionModel", ...)
}

initSorptionK <- function(K, Kmin, Kmax)
{  
  if(nrow(K) != 3) 
    stop("Error in initSorptionK: 'nrow(K) != 3'.")
  if(ncol(K) != 17) 
    stop("Error in initSorptionK: 'ncol(K) != 17'.")

  K <- apply(K, 2, function(x) x * (Kmax - Kmin) + Kmin)
  
  return(K)
}

#----------------------------
# Plot Methods
#----------------------------

#' @rdname plot-methods
#' @aliases plot,SorptionModel-method
setMethod("plot", "SorptionModel", function (x, y, ...) 
{
  yval <- c("response", "heatmap", "data", "predict")
  # missing
  if(missing(y)) y <- "response"
   
  switch(y,
    heatmap = plot.SorptionModel.heatmap(x, y, ...),    
    data = plot.SorptionModel.data(x, y, ...),
    predict = plot.SorptionModel.predict(x, y, ...),
    response = plot.SorptionModel.response(x, y, ...),    
    stop("Error in ConcNoiseModel::plot: plot type 'y' is unknown."))
})

plot.SorptionModel.heatmap <- function(x, y, param="KCmax",
  pal="Blues",
  main = paste("Sorption Model: data, parameter '", param, "'", sep=''), 
  xlab = "Sensors", ylab="Gases", ...)
{
  col <- switch(pal,
    Blues = brewer.pal(9, "Blues"),
    stop("Error in plot.ConcNoiseModel.data: 'pal' is unknown."))
    
  img <- as.matrix(x@srdata[, , param])

  image(img, col=col, axes=FALSE, 
    main=main, xlab=xlab, ylab=ylab, ...)
  axis(1, at=seq(0, 1, length.out=nrow(img)), labels=rownames(img), tick=FALSE)  
  axis(2, at=seq(0, 1, length.out=ncol(img)), labels=colnames(img), las=2, tick=FALSE)  
}

plot.SorptionModel.data <- function(x, y, param="KCmax",
  main = paste("Sorption Model: UNIMAN data '", param, "'", sep=''),  ...)
{
  barplot(x@srdata[, , param], beside=TRUE, main = main)
}

plot.SorptionModel.predict <- function(x, y, n, conc, concUnits="default",
  col, lty = c(1, 3), lwd = 2,
  main = paste("Sorption Model: data"), xlab = "Samples", ylab="Concentration", ...)
{
  if(concUnits == "default") concUnits <- concUnits(x)
  if(missing(n)) n <- 100
  if(missing(conc)) conc <- concSample(x, "inc", n=n, concUnits=concUnits, ...)

  if(missing(main))
    main <- paste(main, "\n knum ", paste(knum(x), collapse=", "), ", non-linearity ", x@alpha, sep="")
  
  nconc <- predict(x, conc, ...)  

  if(missing(col)) col <- gcol(x)
  lty <- rep(lty, each=ngases(x))
  ylab <- paste(ylab, ConcUnitsStr(concUnits), sep=", ")
  
  matplot(cbind(nconc, conc), type = 'l', col = col, lty=lty, lwd=lwd,
    bty='n', 
    main=main, xlab = xlab, ylab = ylab)  
}

plot.SorptionModel.response <- function(x, y,  
  lwd = 2, lty = 1,
  gases = 0, type = "inc", n = 100, 
  xlim = c(0, 1.2),  
  main = "Sorption Model: response", xlab = "Normalized Concentration", ylab="Soprtion Concenration", ...)
{ 
  if(sum(gases == 0)) gases <- gases(x)

  if(missing(main))
    main <- paste(main, "\n knum ", paste(knum(x), collapse=", "), ", non-linearity ", x@alpha, sep="")

  concUnits(x) <- "norm"

  concUnits <- concUnits(x)  
  gind <- gind(x)
  nsensors <- nsensors(x)
    
  yp <- matrix(NA, nrow=n, ncol=length(gases) * nsensors)
  xp <- matrix(NA, nrow=n, ncol=length(gases) * nsensors)
  for(i in 1:length(gases)) {
    g <- gases[i]  
    gi <- gind[g]
    
    conci <- concSample(x, type, n=n, gases=g, concUnits=concUnits)
    
    ind <- seq(1, nsensors) + (i - 1) * nsensors
    xpi <- concNorm(x, conc=conci, concUnits=concUnits, concUnitsInt="norm")[, gi]
    xp[, ind] <- xpi

    ypi <- predict(x, conc=conci, concUnits=concUnits)
    if(nsensors == 1) ypi <- ypi[, gi]
    else ypi <- ypi[, gi, ]
    yp[, ind] <- ypi
  }
  
  col <- gcol(x, gases=rep(gases, each=nsensors))  
  matplot(xp, yp, bty='n', type='l', col=col, 
    lwd = lwd, lty = lty,
    xlim = xlim,
    main=main, xlab = xlab, ylab = ylab)  
}

#----------------------------
# Get/Set Methods
#----------------------------

#----------------------------
# Predict Methods
#----------------------------

#' @rdname model-methods
#' @aliases predict,SorptionModel-method
setMethod ("predict", "SorptionModel", function(object, conc, concUnits="default", ...)
{  
  if(concUnits == "default") concUnits <- concUnits(object)
  if(missing(conc)) 
    stop("Error in SorptionModel::predict: 'conc' is missing.")
  
  if(is.null(ncol(conc))) conc <- matrix(conc, ncol=1) # 1-column case
  if(ncol(conc) != ngases(object))
    stop("Error in SorptionModel::predict: dimension of 'conc' is incorrect.")  

  concUnitsSorption <- concUnitsSorption(object)

  if(concUnits != concUnitsSorption(object)) conc <- concNorm(object, conc, concUnits, concUnitsInt=concUnitsSorption) # concNorm
  conc <- concModel(object, conc=conc, concUnits=concUnitsSorption(object), ...)  # concModel
  if(concUnitsSorption(object) != concUnits) conc <- concDenorm(object, conc, concUnits, concUnitsInt=concUnitsSorption) # concDenorm
  
  return(conc)
})

#----------------------------
# Model Methods
#----------------------------
setMethod("concModel", "SorptionModel", function(object, conc, concUnits="default", ...)
{
  if(concUnits == "default") concUnits <- concUnits(object)
  if(missing(conc)) 
    stop("Error in SorptionModel::predict: 'conc' is missing.")
    
  if(concUnitsSorption(object) != concUnits)
    stop("Error in SorptionModel::concModel: 'concUnits' is different from slot 'concUnitsSorption'.")    
 
  nsensors <- nsensors(object)
  gind <- gind(object)
  n <- nrow(conc)
  
  # rows == samples, columns == idx
  conc.sum <- t(apply(conc, 1, function(x) apply(object@sorptionModel$K * x, 2, sum)))
  if(nsensors == 1) conc.sum <- matrix(conc.sum, ncol=1) # 1-column case

  conc.out <- array(NA, c(dim(conc), nsensors))
  for(i in idx(object)) {
    for(ii in 1:n) {
      conc.out[ii, , i] <- conc[ii, ] * object@sorptionModel$Q[, i] * object@sorptionModel$K[, i] / (1 + conc.sum[ii, i])
    }
  }
  
  if(nsensors == 1) conc.out <- conc.out[, , 1]
  if(is.null(ncol(conc.out))) conc.out <- matrix(conc.out, ncol=1) # 1-column case
  
  if(n == 1 & nsensors == 1) conc.out <- matrix(conc.out, nrow=1)
    
  return(conc.out)
})
