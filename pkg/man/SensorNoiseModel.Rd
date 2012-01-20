\name{SensorNoiseModel}
\alias{SensorNoiseModel}
\title{Class SensorNoiseModel.}
\description{
  Class \code{\link{SensorNoiseModel}} generates a sensor
  noise.
}
\details{
  Slots of the class: \tabular{rl}{ \code{ssd} \tab
  Parameter of standard deviation used to generate the
  noise. \cr \code{sntype} \tab Noise type. One type is
  implemented, 'randomWalk'. \cr \code{snf} \tab Scaling
  factor for the amplitude to generate the noise.  The
  default value is c(1, 1, 0.2). \cr \code{sndata} \tab The
  reference data of standard deviation values from UNIMAN
  dataset. \cr }
}
\examples{

sn <- SensorNoiseModel()

set.seed(6)
plot(sn, "walk", n=100, k=5)
  
}
\keyword{SensorNoiseModel-class}

