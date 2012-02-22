\name{SensorDynamics}
\alias{SensorDynamics}
\title{Class SensorDynamics.}
\description{
  Class \code{\link{SensorDynamics}} emulates a temporal
  dynamics of the sensor as a low-pass filter.
}
\details{
  Slots of the class: \tabular{rl}{ \code{num} \tab Sensor
  number (\code{1:17}). The default value is \code{1}. \cr
  \code{gases} \tab Gas indices. \cr \code{ngases} \tab The
  number of gases. \cr \code{gnames} \tab Names of gases.
  \cr \code{concUnits} \tab Concentration units external to
  the model. \cr \code{concUnitsInt} \tab Concentration
  units internal to the model. \cr \code{tunit} \tab Time
  length of the gas pulses. The default value is \code{60}.
  \cr \code{enableDyn} \tab A logical value indicates
  whether time dynamics is enabled. It is \code{TRUE} if
  \code{tunit} is greater than one. \cr \code{tconst} \tab
  Time constants of the low pass filter. \cr }

  Methods of the class: \tabular{rl}{ \code{predict} \tab
  Predicts a temporal response of the sensor. \cr
  \code{coef} \tab Extracts the time constants of the
  filter. \cr }

  The \code{plot} method has one type (parameter \code{y}):
  \tabular{rl}{ \code{predict} \tab Depicts a temporal
  signal of the model. \cr }
}
\examples{
# sensor dynamics: default initialization
sdyn <- SensorDynamics()

show(sdyn)
print(sdyn)

plot(sdyn)

# SensorDynamics as a part of SensorModel
sm <- SensorModel(tunit=60)
sdyn <- as(sm, "SensorDynamics")
}
\seealso{
  \code{\link{UNIMANtransient}}
}
\keyword{SensorDynamics}

