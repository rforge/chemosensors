
data("UNIMANshort", package="chemosensors")

str(UNIMANshort)

C <- UNIMANshort$C
dat <- UNIMANshort$dat

# plot sensors in affinity space of gases
#plotAffinitySpace(conc=C, sdata=dat, gases=c(2, 1))
#plotAffinitySpace(conc=C, sdata=dat, gases=c(2, 3))
#plotAffinitySpace(conc=C, sdata=dat, gases=c(3, 1))

# make standar PCA (package 'pls') to see:
# - multi-variate class distribution (scoreplot)
# - low-dimensionality of data (variance)
# - contribution of 17 sensors in terms of linear modeling (loadings)
mod <- prcomp(dat, center=TRUE, scale=TRUE)

col <- ccol(C)
scoreplot(mod, col=col, main="PCA: Scoreplot")

barplot(mod$sdev, main="PCA: Sd. Deviation ~ PCs")

loadings <- mod$rotation
col <- grey.colors(3, start=0.3, end=0.9)
matplot(loadings[, 1:3], t='l', col=col, lwd=2, lty=1,
  xlab="Sensor", ylab="sdev", main="PCA: Loadings PCs 1-3 ~ Sensors")
