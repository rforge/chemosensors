\name{SensorArrayModel}
\alias{SensorArrayModel}
\title{Class SensorArrayModel.}
\description{
  Class \code{\link{SensorArrayModel}} is a extension of
  the class \code{\link{SensorModel}} for many sensor
  elements.
}
\details{
  In comparision to the class \code{\link{SensorModel}},
  slot \code{dataModel} is a list containig the data models
  of class \code{\link{SensorDataModel}}.

  See \code{\link{SensorModel}} for more details.

  Slots of the class: \tabular{rl}{ \code{num} \tab Sensor
  number (\code{1:17}). The default value is \code{c(1,
  2)}. \cr \code{gases} \tab Gas indices. \cr \code{ngases}
  \tab The number of gases. \cr \code{gnames} \tab Names of
  gases. \cr \code{concUnits} \tab Concentration units
  external to the model, values given in an input
  concentration matrix. \cr \code{concUnitsInt} \tab
  Concentration units internal for the model, values used
  numerically to build regression models. \cr
  \code{dataModel} \tab List of data models of class
  \code{\link{SensorDataModel}}. \cr \code{coeffNonneg}
  \tab Logical whether model coefficients must be
  non-negative. By default, \code{FALSE}. \cr
  \code{coeffNonnegTransform} \tab Name of transformation
  to convert negative model coefficients to non-negative
  values. \cr }

  Methods of the class: \tabular{rl}{ \code{predict} \tab
  Predicts a model response to an input concentration
  matrix. \cr \code{coef} \tab Extracts the coefficients of
  models stored in slot \code{dataModel}. \cr }

  The \code{plot} method has the only type (parameter
  \code{y}): \tabular{rl}{ \code{response} \tab (default)
  Shows the sensitivity curves per gas in normalized
  concentration units. \cr }
}
\examples{

# model: default initialization
sam <- SensorArrayModel()

# get information about the model
show(sam)
print(sam)

print(coef(sam)) # sensitivity coefficients

plot(sam)  

# model: custom parameters
sam <- SensorArrayModel(num=15:17, model="plsr", gases=c(1, 3))

print(sam)

plot(sam, uniman=TRUE) # add UNIMAN reference data (the model was build from)

# method plot
#  - plot types 'y': response
sam <- SensorArrayModel() # default sensor model

plot(sam, "response", main="plot(sam, 'response')") # default plot type, i.e. 'plot(sam)' does the same plotting  
}
\seealso{
  \code{\link{SensorModel}}
}
\keyword{SensorArrayModel}

