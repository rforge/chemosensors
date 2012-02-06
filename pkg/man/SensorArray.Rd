\name{SensorArray}
\alias{SensorArray}
\title{Class SensorArray.}
\description{
  Class \code{SensorArray} is a extension of the class
  \code{\link{Sensor}} for many sensor elements.
}
\details{
  The array aggregates classes
  \code{\link{ConcNoiseModel}},
  \code{\link{SensorNoiseModel}},
  \code{\link{SorptionModel}},
  \code{\link{SensorArrayModel}} and
  \code{\link{DriftNoiseModel}}.

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
  \code{\link{SorptionModel}},
  \code{\link{SensorArrayModel}} and
  \code{\link{DriftNoiseModel}}. \cr }

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

plot(sa)  

# model: custom parameters
sa <- SensorArray(num=1:17) # 17 UNIMAN virtual sensors
plot(sa, main="17 UNIMAN virtual sensors")

sa <- SensorArray(num=15:17, Knonlin=0.01, model="plsr", gases=c(1, 3)) # array with quite linear sensors

print(sa)

plot(sa, uniman=TRUE, main="Array of more linear sensors") # add UNIMAN reference data (the models were build from)

# method plot
#  - plot types 'y': response
sa <- SensorArray() # default sensor model

plot(sa, "response", main="plot(sa, 'response')") # default plot type, i.e. 'plot(sa)' does the sae plotting  
}
\seealso{
  \code{\link{Sensor}}, \code{\link{DriftNoiseModel}}
}
\keyword{SensorArray}

