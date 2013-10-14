\name{SorptionModel-class}
\alias{SorptionModel}
\alias{SorptionModel-class}
\alias{concUnitsSorption}
\alias{concUnitsSorption,SorptionModel-method}
\alias{defaultSorptionModel}
\alias{knum}
\alias{knum,SorptionModel-method}
\title{Method knum.}
\usage{
  defaultSorptionModel()

  SorptionModel(...)
}
\arguments{
  \item{...}{parameters of constructor.}
}
\value{
  List of the default parameters.
}
\description{
  Method knum.

  Method concUnitsSorption.

  Class \code{\link{SorptionModel}} controls the the amount
  of gas absorbed by the sensor, that emulates the
  non-linearity nature intrinsic for the polymeric sensors.

  Function to get default constructor parameters of class
  \code{\link{SorptionModel}}.

  Constructor method of SorptionModel Class.

  Wrapper function SorptionModel.
}
\details{
  The model is based on the extended Langmuir isotherm for
  a multi-component gas mixture, and has two parameters per
  analyte, \code{Q} denotes the sorption capacity and
  \code{K} stands for the sorption affinity.

  Slots of the class: \tabular{rl}{ \code{knum} \tab \cr
  \code{sorptionModel} \tab List of model parameters 'K'
  and 'Q'. \cr } Slots of the class: \tabular{rl}{
  \code{knum} \tab Sensor number that encodes a UNIMAN
  sorption profile (\code{1:17}). The default value is
  \code{1}. \cr \code{gases} \tab Gas indices. \cr
  \code{ngases} \tab The number of gases. \cr \code{gnames}
  \tab Names of gases. \cr \code{concUnits} \tab
  Concentration units external to the model, values given
  in an input concentration matrix. \cr \code{concUnitsInt}
  \tab Concentration units internal for the model, values
  used numerically to evaluate the Langmuir relation. \cr
  \code{sorptionModel} \tab A list that contains the
  Langmuir parameters. \cr \code{srdata} \tab The reference
  data of Lanmuir parameters from UNIMAN dataset (see
  \code{\link{UNIMANsorption}}). \cr \code{alpha} \tab
  (parameter of sensor non-linearity in mixtures) A scaling
  coefficient of non-linearity induced via the affinity
  parameter \code{K}. The default value is \code{2.25}. \cr
  }

  Methods of the class: \tabular{rl}{ \code{predict} \tab
  Predicts a model response to an input concentration
  matrix. \cr }

  The \code{plot} method has three types (parameter
  \code{y}): \tabular{rl}{ \code{response} \tab (default)
  Shows a modeled trasnformation of concentration profile
  per analyte. \cr \code{data} \tab Shows the reference
  data from UNIMAN dataset. \cr \code{predict} \tab Depicts
  input and ouput of the model for all analytes. \cr }
}
\note{
  We introduce a single parameter \code{alpha} of the model
  to control the level of non-linearity simulated by the
  Langmuir isotherm. This parameter \code{alpha} defines a
  normalization across the 17 UNIMAN sorption profiles from
  dataset \code{\link{UNIMANsorption}}, scaling \code{K}
  values based on other two parameters \code{Kmin} (default
  value \code{1} and \code{Kmax} (default value
  \code{150}). Normalization can be disable by setting
  parameter \code{Knorm} to \code{FALSE}, that results in
  usage of the sorption \code{K} parameters, equal to
  UNIMAN ones.
}
\examples{
# sorption model: default initialization
sm <- SorptionModel()

# get information about the model
show(sm)
print(sm)

plot(sm)  

# model: custom parameters
# almost linear model
sm <- SorptionModel(alpha=0.01, gases=c(1, 3)) 
plot(sm, main="Almost linear model, non-linearity 0.01")

# non-linear model
sm <- SorptionModel(alpha=0.3, gases=c(1, 3)) 
plot(sm, main="Non-linear model, non-linearity 0.5")

# saturated model
sm <- SorptionModel(alpha=0.5, gases=c(1, 3)) 
plot(sm, main="Saturated model, non-linearity 1")

# model with UNIMAN sorption parameters
sm <- SorptionModel(Knorm=FALSE, gases=c(1, 3)) 
plot(sm, main="Model with UNIMAN parameters (no normalization)")

# method plot
#  - plot types 'y': response, data, predict
sm <- SorptionModel() # default model

plot(sm, "response", main="plot(sm, 'response')") 
# default plot type, i.e. 'plot(sm)' does the same plotting

plot(sm, "data", main="plot(sm, 'data')")

plot(sm, "predict", main="plot(sm, 'predict')")
}
\seealso{
  \code{\link{UNIMANsorption}}
}

