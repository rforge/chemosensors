\name{Sensor}
\alias{Sensor}
\title{Class Sensor.}
\description{
  Class \code{\link{Sensor}} predicts a sensor signal in
  response to an input concentration matrix by passing the
  data through a list of models.
}
\details{
  The models aggregated into the class are
  {\link{ConcNoiseModel}}, {\link{SensorNoiseModel}} and
  {\link{SensorModel}}.

  Slots of the class: \tabular{rl}{ \code{type} \tab Sensor
  type. Default value is 'polymeric'. \cr \code{idx} \tab
  Sensor index (unique ID number). \cr }
}
\examples{


# Fit to UNIMAN Data
pck <- defaultDataPackage()
data(UNIMANshort, package=pck)

sm <- SensorModel()
plot(sm, uniman=TRUE, C=C, X=dat, jitter=TRUE)

s <- Sensor()
plot(s, uniman=TRUE, C=C, X=dat, jitter=TRUE)


# Non-liearity in a sensor
#  - class structure: Sensor = SorptionModel + SensorModel 
sm <- SensorModel()
plot(sm)

s <- Sensor()
plot(s)


# Sensor noise
for(ssd in c(0.01, 0.1, 1)) {
  s <-  Sensor(ssd=ssd)
  plot(s, "snoise")  
}

}
\keyword{Sensor-class}

