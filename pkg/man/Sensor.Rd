\name{Sensor}
\alias{Sensor}
\title{Class Sensor.}
\description{
  Class \code{\link{Sensor}} predicts a sensor signal in
  response to an input concentration matrix by calling two
  sub-models \code{\link{SorptionModel}} and
  \code{\link{SensorModel}}.
}
\details{
  The sensor aggregates classes
  \code{\link{ConcNoiseModel}},
  \code{\link{SensorNoiseModel}},
  \code{\link{SorptionModel}} and
  \code{\link{SensorModel}}.

  The sorption and calibration models (classes
  \code{\link{SorptionModel}} and
  \code{\link{SensorModel}}) have different roles in the
  simulation flow. The sorption model controls the the
  amount of gas absorbed by the sensor following the
  Langmuir relation, and that emulates the non-linearity
  nature intrinsic for the polymeric sensors. The
  calibration model regulates the relationship between this
  amount of the absorbed gas and the output sensor signal.
  Both sorption and calibration models utilize the
  short-term reference data \code{\link{UNIMANshort}} for
  parameter estimation. The sorption model can be disabled
  by parameter \code{enableSorption}.

  Slots of the class: \tabular{rl}{ \code{type} \tab Sensor
  type (not used). Default value is \code{polymeric}. \cr
  \code{idx} \tab Sensor index (unique ID number). \cr
  \code{enableSorption} \tab Boolean whether
  \code{\link{SorptionModel}} is enabled. Default value is
  \code{TRUE}. \cr \code{...} \tab Slots inherited from
  super-classes \code{\link{ConcNoiseModel}},
  \code{\link{SensorNoiseModel}},
  \code{\link{SorptionModel}} and
  \code{\link{SensorModel}}. \cr }

  Methods of the class: \tabular{rl}{ \code{predict} \tab
  Predicts a sensor response to an input concentration
  matrix. \cr \code{coef} \tab Extracts the coefficients
  inherited from class \code{\link{SensorModel}}. \cr
  \code{csd} \tab Gets the concentration noise level
  (inherited from class \code{\link{ConcNoiseModel}}). \cr
  \code{csd<-} \tab Sets the concentration noise level. \cr
  \code{ssd} \tab Gets the sensor noise level (inherited
  from class \code{\link{SensorNoiseModel}}). \cr
  \code{ssd<-} \tab Sets the sensor noise level. \cr }

  The \code{plot} method has two types (parameter
  \code{y}): \tabular{rl}{ \code{response} \tab (default)
  Shows the sensitivity curves per gas in normalized
  concentration units. \cr \code{noise} \tab Depicts the
  noise induced in the output signal of the sensor. \cr }
}
\examples{
# sensor object: default initialization
s <- Sensor()

# get information about the sensor
show(s)
print(s)

plot(s)  

# sensor object: custom parameters
s <- Sensor(num=5, enableSorption=FALSE) # sorption model disabled
plot(s, main="Sensor with sorption model disabled")

s <- Sensor(num=5, Knonlin=0.01, gases=c(1, 3)) # amost linear sensor
plot(s, main="Almost linear sensor, non-linearity 0.01")

s <- Sensor(num=5, Knonlin=1, gases=c(1, 3)) # saturated sensor
plot(s, main="Saturated sensor, non-linearity 1")

s <- Sensor(num=5, csd=0, ssd=0) # noise level is set to zero
plot(s, "noise", main="Noise-free sensor")

s <- Sensor(num=5, csd=1, ssd=1) # maximum reasonable level of noise
plot(s, "noise", main="Very noisy sensor")

# method plot
#  - plot types 'y': response, noise
s <- Sensor() # default model

plot(s, "response", main="plot(s, 'response')") # default plot type, i.e. 'plot(s)' does the same plotting

plot(s, "noise", main="plot(s, 'noise')")

}
\seealso{
  \code{\link{ConcNoiseModel}},
  \code{\link{SensorNoiseModel}},
  \code{\link{SorptionModel}}, \code{\link{SensorModel}}
}
\keyword{Sensor}

