
data(UNIMANdnoise)

str(UNIMANdnoise)

# 17 sensor loadings, 17 drift components
dim(UNIMANdnoise$dspace)

# importance of 17 drift components
head(UNIMANdnoise$ndvar)

barplot(UNIMANdnoise$ndvar, names.arg=1:length(UNIMANdnoise$ndvar), 
  main="Importance of drift components")
# comp. 1 dominates, 
# comp. 1-3 dominate over the rest

loadings <- UNIMANdnoise$dspace
col <- grey.colors(3, start=0.3, end=0.9)
matplot(loadings[, 1:3], t='l', col=col, lwd=2, lty=1,
  xlab="Sensors", ylab="Loadings", 
  main="Loadings of sensors on first 3 drift components")
# sensors 7, 8, 9, 17 seem to be less drifty  
