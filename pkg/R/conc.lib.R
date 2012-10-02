
#' @include ChemosensorsClass.R
NULL

#----------------------------
# Conc Methods
#----------------------------

### Method conc2dat
setMethod("conc2df", "ANY", function(object, conc, ...)
{  
  if(missing(conc))
    stop("Error in ANY::conc2df: 'conc' is missing.")
    
  gases <- gases(object)
  gnames <- gnames(object)
  ngases <- ngases(object) 
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

### Method conc2lab
setMethod("conc2lab", "ANY", function(object, ...)
{  
  df <- conc2df(object, ...)
  lab <- df$lab
  
  return(lab)
})

### Method conc2glab
setMethod("conc2glab", "ANY", function(object, ...)
{  
  df <- conc2df(object, ...)
  glab <- df$glab
  
  return(glab)
})

#----------------------------
# Conc2Col Methods
#----------------------------

### Method conc2col
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


### Method conc2tpoint
# "airin"  "air"    "airout" "gasin"  "gas"    "gasout"
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








