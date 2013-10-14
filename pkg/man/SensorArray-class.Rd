\name{SensorArray-class}
\alias{Sensor}
\alias{SensorArray}
\alias{SensorArray-class}
\alias{affinity}
\alias{affinity,ANY-method}
\alias{defaultParSensorArray}
\alias{getSensor}
\alias{getSensor,SensorArray-method}
\title{Method getSensor.}
\usage{
  defaultParSensorArray()

  SensorArray(...)

  Sensor(num = 1, ...)
}
\arguments{
  \item{...}{parameters of constructor.}

  \item{num}{Type of sensors (or UNIMAN number).}
}
\value{
  List of the default parameters.
}
\description{
  Method getSensor.

  Method affinity.

  Class \code{SensorArray} is a extension of the class
  \code{\link{Sensor}} for many sensor elements.

  Function to get default constructor parameters of class
  \code{\link{SensorArray}}.

  Constructor method of SensorArray Class.

  Wrapper function SensorArray.

  Wrapper function Sensor
}
\details{
  The array aggregates classes
  \code{\link{ConcNoiseModel}},
  \code{\link{SensorNoiseModel}},
  \code{\link{SorptionModel}}, \code{\link{SensorModel}}
  and \code{\link{DriftNoiseModel}}.

  In comparision to the class \code{\link{Sensor}}, slot
  \code{num} is a numeric vector, and class
  \code{SensorArray} also inherits class
  \code{\link{DriftNoiseModel}}.

  See \code{\link{Sensor}} and
  \code{\link{DriftNoiseModel}} for more details.

  Slots of the class: \tabular{rl}{ \code{type} \tab Sensor
  type (not used). Default value is \code{polymeric}. \cr
  \code{idx} \tab Sensor index (unique ID number). \cr
  \code{enableSorption} \tab Boolean whether
  \code{\link{SorptionModel}} is enabled. Default value is
  \code{TRUE}. \cr \code{...} \tab Slots inherited from
  super-classes \code{\link{ConcNoiseModel}},
  \code{\link{SensorNoiseModel}},
  \code{\link{SorptionModel}}, \code{\link{SensorModel}}
  and \code{\link{DriftNoiseModel}}. \cr }

  Methods of the class: \tabular{rl}{ \code{predict} \tab
  Predicts a model response to an input concentration
  matrix. \cr \code{coef} \tab Extracts the coefficient
  matrix from sensors. \cr \code{csd} \tab Gets the
  concentration noise level (inherited from class
  \code{\link{ConcNoiseModel}}). \cr \code{csd<-} \tab Sets
  the concentration noise level. \cr \code{ssd} \tab Gets
  the sensor noise level (inherited from class
  \code{\link{SensorNoiseModel}}). \cr \code{ssd<-} \tab
  Sets the sensor noise level. \cr }

  The \code{plot} method has the only type (parameter
  \code{y}): \tabular{rl}{ \code{response} \tab (default)
  Shows the sensitivity curves per gas in normalized
  concentration units. \cr }
}
\examples{

# array: default initialization
sa <- SensorArray()

# get information about the array
show(sa)
print(sa)

print(coef(sa)) # array coefficients

#plot(sa)  

# model: custom parameters
sa <- SensorArray(num=1:17) # 17 UNIMAN virtual sensors
plot(sa, main="17 UNIMAN virtual sensors")

# array with quite linear sensors
sa <- SensorArray(num=15:17, alpha=0.01, model="mvr") 
print(sa)

# add UNIMAN reference data (the models were build from)
p1 <- plotResponse(sa, main="Array of more linear sensors") 
# sensor object: default initialization
s <- Sensor()

# get information about the sensor
show(s)
print(s)

plot(s)  

# sensor object: custom parameters
s <- Sensor(num=5, enableSorption=FALSE) # sorption model disabled
plot(s, main="Sensor with sorption model disabled")

s <- Sensor(num=5, alpha=0.01) # amost linear sensor
plot(s, main="Almost linear sensor, non-linearity 0.01")

s <- Sensor(num=5, alpha=1) # saturated sensor
plot(s, main="Saturated sensor, non-linearity 1")

s <- Sensor(num=5, csd=0, ssd=0, dsd = 0) # noise level is set to zero
plot(s, "snoise", main="Noise-free sensor")

s <- Sensor(num=5, csd=1, ssd=1, dsd = 0) # maximum reasonable level of noise
plot(s, "snoise", main="Very noisy sensor")

# method plot
#  - plot types 'y': response, noise
s <- Sensor() # default model

plot(s, "response", main="plot(s, 'response')") 
# default plot type, i.e. 'plot(s)' does the same plotting

plot(s, "snoise", main="plot(s, 'snoise')")

}
\seealso{
  \code{\link{Sensor}}, \code{\link{DriftNoiseModel}}

  \code{\link{SensorArray}}, \code{\link{SensorModel}}
}

