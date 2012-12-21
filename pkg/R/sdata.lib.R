
#' @include ChemosensorsClass.R
NULL

#----------------------------
# Sdata Methods
#----------------------------

### Method sdata2pulse
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








