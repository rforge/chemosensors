\name{SensorDynamics-class}
\alias{SensorDynamics}
\alias{SensorDynamics-class}
\alias{defaultParSensorDynamics}
\alias{nconst}
\alias{nconst,SensorDynamics-method}
\alias{tconst}
\alias{tconst,SensorDynamics-method}
\alias{trange}
\alias{trange,SensorDynamics-method}
\title{Method nconst.}
\usage{
  defaultParSensorDynamics()

  SensorDynamics(...)
}
\arguments{
  \item{...}{parameters of constructor.}
}
\value{
  List of the default parameters.
}
\description{
  Method nconst.

  Method trange.

  Method tconst.

  Class \code{\link{SensorDynamics}} emulates a temporal
  dynamics of the sensor as a low-pass filter.

  Function to get default constructor parameters of class
  \code{\link{SensorDynamics}}.

  Constructor method of SensorDynamics Class.

  Wrapper function SensorDynamics.
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
### Sensor dynamics: default initialization
sdyn <- SensorDynamics()

sdyn # equivalent to: show(sdyn)

print(sdyn)

#plot(sdyn)

### SensorDynamics as a part of SensorModel
sm <- SensorModel(tunit = 60)
sdyn <- as(sm, "SensorDynamics")

### Demo 1: response to pulses of different concentrations
set.seed(5)
sa <- SensorArray(tunit = 60, csd = 1, ssd = 0, dsd = 0)

# mixture at low concentrations
p1 <- plotSignal(sa, set = c("A 0.01", "C 0.1", "A 0.01, C 0.1"), main = "Low concentrations")
p1

# mixture at high concentrations
p2 <- plotSignal(sa, set = c("A 0.05", "C 1", "A 0.05, C 1"), main = "High concentrations")
p2

###
sa <- SensorArray(tunit = 60, nsd = 0)

p3 <- plotSignal(sa, set = c("A", "B", "C"))
p3

p4 <- plotPCA(sa, set = c("A", "B", "C"))
p4
}
\seealso{
  \code{\link{UNIMANtransient}}
}

