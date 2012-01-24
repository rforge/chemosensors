\name{DriftNoiseModel}
\alias{DriftNoiseModel}
\title{Class DriftNoiseModel.}
\description{
  Class \code{\link{DriftNoiseModel}} generates the drift
  noise in a multi-variate manner.
}
\details{
  Slots of the class: \tabular{rl}{ \code{num} \tab Sensor
  number (1:17). The default value is 1. \cr \code{dsd}
  \tab Parameter sd for drift noise. A number from 0 to 1.
  The default value is 0.1. \cr \code{ndcomp} \tab The
  number of drift components. The default number is 3. \cr
  \code{ndvar} \tab The importance values of drift
  components. The default values are taken from the UNIMAN
  multi-variate distribution. \cr \code{driftModel} \tab
  Drift model of class \code{\link{DriftCommonModel}}. \cr
  }
}
\examples{

data(UNIMANshort, package="chemosensors") # -> 'dat', 'C'

# case 1: 17 sensors
# all types of sensors
# drift components are orthogonal
num <- 1:17
X <- dat[, num]

dn <- DriftNoiseModel(num=num)
plot(dn, "pc", X)

# case 2: 5 sensors
# 5 types of sensors
# drift components are overlaping (not orthogonal)
num <- 1:5
X <- dat[, num]

dn <- DriftNoiseModel(num=num)
plot(dn, "pc", X)

# case 3: 17 + 5 sensors
# drift components are orthogonal
num <- c(1:17, 1:5)
X <- dat[, num]

dn <- DriftNoiseModel(num=num)
plot(dn, "pc", X)
}
\keyword{DriftNoiseModel-class}

