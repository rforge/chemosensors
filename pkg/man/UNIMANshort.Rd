\docType{data}
\name{UNIMANshort}
\alias{UNIMANshort}
\title{Dataset UNIMANshort.}
\description{
  Short-term UNIMAN datasets of 200 samples from 17
  polymeric sensors. The datasets contains two matricies:
  \tabular{rl}{ \code{C} \tab The concentration matrix of
  200 rows and 3 columns encodes the concentration profile
  for three gases, ammonia, propanoic acid and n-buthanol.
  The concentration units are given in the percentage
  volume (\% vol.).  Ammonia has three concentration levels
  0.01, 0.02 and 0.05, propanoic acid - three levels 0.01,
  0.02 and 0.05, and n-buthanol - two levels 0.1 and 1. \cr
  \code{dat} \tab The data matrix of 200 rows and 17
  columns cotains the steady-state signals of 17 sensors in
  response to the concentration profle \code{C}. \cr }
}
\details{
  The reference dataset has been measured at The University
  of Manchester (UNIMAN). Three analytes ammonia, propanoic
  acid and n-buthanol, at different concentration levels,
  were measured for 10 months with an array of seventeen
  conducting polymer sensors.

  In modeling of the array we make the distinction between
  short-term and long-term reference data. Two hundred
  samples from the first 6 days are used to characterize
  the array assuming the absence of drift. The long-term
  reference data (not published within the package) counts
  for the complete number of samples from 10 months, these
  data were used to model the sensor noise and drift, see
  \code{\link{UNIMANsnoise}} and \code{\link{UNIMANdnoise}}
  for more details.

  A pre-processing procedure on outliers removal was
  applied to the reference data. The standard method based
  on the squared Mahalanobis distance was used with
  quantile equal to \code{0.975}\%.
}
\examples{

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
}
\seealso{
  \code{\link{SensorModel}}, \code{\link{SensorModel}}
}
\keyword{data}
\keyword{datasets}

