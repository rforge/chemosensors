\name{SensorModel-class}
\alias{SensorModel}
\alias{SensorModel-class}
\alias{SensorModelNames}
\alias{coeffNonneg}
\alias{coeffNonneg,SensorModel-method}
\alias{defaultParSensorModel}
\title{Method coeffNonneg.}
\usage{
  SensorModelNames()

  defaultParSensorModel()

  SensorModel(...)
}
\arguments{
  \item{...}{parameters of constructor.}
}
\value{
  Character vector of model names.

  List of the default parameters.
}
\description{
  Method coeffNonneg.

  Class \code{\link{SensorModel}} predicts a sensor signal
  in response to an input concentration matrix by means of
  a regression model stored in slot \code{dataModel}.

  Function to get model names of class
  \code{\link{SensorModel}}.

  Function to get default constructor parameters of class
  \code{\link{SensorModel}}.

  Constructor method of SensorModel Class.

  Wrapper function SensorModel.
}
\details{
  The model explicitely assumes that the sensor response to
  a mixture of analytes is a sum of responses to the
  individual analyte components. Linear models \code{mvr}
  and \code{plsr} follow this assumtion in their nature.

  Slots of the class: \tabular{rl}{ \code{num} \tab Sensor
  number (\code{1:17}). The default value is \code{1}. \cr
  \code{gases} \tab Gas indices. \cr \code{ngases} \tab The
  number of gases. \cr \code{gnames} \tab Names of gases.
  \cr \code{concUnits} \tab Concentration units external to
  the model, values given in an input concentration matrix.
  \cr \code{concUnitsInt} \tab Concentration units internal
  for the model, values used numerically to build
  regression models. \cr \code{dataModel} \tab Data model
  of class \code{SensorDataModel} performs a regression
  (free of the routine on units convertion, etc). \cr
  \code{coeffNonneg} \tab Logical whether model
  coefficients must be non-negative. By default,
  \code{FALSE}. \cr \code{coeffNonnegTransform} \tab Name
  of transformation to convert negative model coefficients
  to non-negative values. \cr \code{beta} \tab (parameter
  of sensor diversity) A scaling coefficient of how
  different coefficients of \code{SensorDataModel} will be
  in comparision with those coefficients of the UNIMAN
  sensors.  The default value is \code{2}. \cr }

  Methods of the class: \tabular{rl}{ \code{predict} \tab
  Predicts a sensor model response to an input
  concentration matrix. \cr \code{coef} \tab Extracts the
  coefficients of a regression model stored in slot
  \code{dataModel}. \cr }

  The \code{plot} method has two types (parameter
  \code{y}): \tabular{rl}{ \code{response} \tab (default)
  Shows the sensitivity curves per gas in normalized
  concentration units. \cr \code{predict} \tab Depicts
  input (parameter \code{conc}) and ouput of the model for
  a specified gas (parameter \code{gases}). \cr }
}
\examples{

# sensor model: default initialization
sm <- SensorModel()

# get information about the model
show(sm)
print(sm)

print(coef(sm)) # sensitivity coefficients

plot(sm)  

# get available model names
model.names <- SensorModelNames()
print(model.names)

# sensor model: custom parameters
sm <- SensorModel(num=7, model="plsr", gases=c(1, 3))

print(sm)

#plot(sm, uniman=TRUE) # add UNIMAN reference data (the model was build from)

# method plot
#  - plot types 'y': response, predict
sm <- SensorModel() # default sensor model

plot(sm, "response", main="plot(sm, 'response')") 
# default plot type, i.e. 'plot(sm)' does the same plotting

conc <- concSample(sm, "range", gases=1, n=10)
plot(sm, "predict", conc, gases=1, main="plot(sm, 'predict', conc, gases=1)")
}
\seealso{
  \code{\link{UNIMANshort}}
}

