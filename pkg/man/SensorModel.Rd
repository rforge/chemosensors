\name{SensorModel}
\alias{SensorModel}
\title{Class SensorModel.}
\description{
  Class \code{\link{SensorModel}} predicts a sensor signal
  in response to an input concentration matrix by means of
  a data model in slot \code{model}.
}
\details{
  The training of the data model is based on the reference
  data from a dataset measured at The University of
  Manchester (UNIMAN) for three analytes ammonia, propanoic
  acid and n-buthanol at different concentration levels.

  Slots of the class: \tabular{rl}{ \code{num} \tab Sensor
  number (1:17). The default value is 1. \cr \code{gases}
  \tab Gas indices. \cr \code{ngases} \tab The number of
  gases. \cr \code{gnames} \tab Names of gases. \cr
  \code{concUnits} \tab Concentration units external to the
  model, values given in an input concentration matrix. \cr
  \code{concUnitsInt} \tab Concentration units internal for
  the model. \cr \code{conc} \tab List of characteristic
  concentration values per gas (in the adsorbed form):
  \code{min}, \code{max}, \code{crit}, \code{sat}. \cr
  \code{conc0} \tab List of characteristic concentration
  values per gas (in air): \code{min}, \code{max},
  \code{crit}, \code{sat}. \cr \code{dataModel} \tab Data
  model of class \code{\link{SensorDataModel}}. \cr
  \code{coeffNonneg} \tab Logical whether model
  coefficients must be non-negative. By default, FALSE. \cr
  \code{coeffNonnegTransform} \tab Name of transformation
  to convert negative model coefficients to non-negative
  values. \cr }
}
\keyword{SensorModel-class}

