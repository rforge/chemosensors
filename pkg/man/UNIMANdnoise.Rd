\docType{data}
\name{UNIMANdnoise}
\alias{UNIMANdnoise}
\title{Dataset UNIMANdnoise.}
\description{
  The dataset contains statistics on a multi-variate drift
  sub-space for the long-term UNMIAN dataset of 1000
  samples over 8 classes.
}
\details{
  The subspace was evaluated via common principal component
  analysis (power algorithm). Only three classes (analytes
  at maximum concentration) are used in the computation for
  more accurate estimation. The importance of the drift
  components is computed as a projected variance of sensor
  array data of the given three classes onto the
  components.

  The datasets contains four variables: \tabular{rl}{
  \code{dspace} \tab A square \code{17 x 17} matrix of the
  drift subspace. Columns are drift component vectors. \cr
  \code{ndvar} \tab A vector of length \code{17} with the
  importance of the components.\cr \code{centered} \tab
  Boolean indicating whether the UNIMAN data were centered
  before the computation. \cr \code{scaled} \tab Boolean
  indicating whether the UNIMAN data were scaled before the
  computation. \cr }
}
\examples{

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
}
\seealso{
  \code{\link{DriftNoiseModel}}
}
\keyword{data}
\keyword{datasets}

