\docType{data}
\name{demo-NeuromorphicSim}
\alias{demo-NeuromorphicSim}
\title{Demo NeuromorphicSim.}
\description{
  This demonstration shows an example of neuromorphic
  simulations possible with package
  \code{\link{chemosensors}}. Particularly, we perform a
  mixture quantification scenario, where the system is
  trained on pure analytes and validation is thought to
  quantify concentration of components in mixtures.
}
\details{
  The user should be aware of the following issues when
  implementing such simulations. \itemize{ \item{A
  concentration matrix (of three analytes maximum) can be
  coded in several ways, manually by function \code{matrix}
  or by using methods of class \code{Scenario}.  See the
  example code on class help page \code{\link{Scenario}}
  for more details}.  \item{To configure a sensor array,
  there are a list of parameters for initialization method
  of class \code{\link{SensorArray}}: the number of sensors
  (\code{nsensors}), sensor types (\code{num}), sensor
  non-linearity (\code{alpha}), sensor diversity
  (\code{beta}), noise levels (\code{csd}, \code{ssd} and
  \code{dsd}), and sensor dynamics (\code{enableDyn}).}
  \item{To parallelize the computation in data generation
  process, one can specify the parameter \code{nclusters}
  (default value is \code{1}) of method \code{predict}.
  Another alternative is to set a global option
  \code{cores} by command \code{options(cores = 2)}.} }
}
