\name{Scenario}
\alias{Scenario}
\title{Class Scenario.}
\description{
  Class \code{Scenario} represents a concentration profile
  to build a concentration matrix.
}
\details{
  Slots of the class: \tabular{rl}{ \code{tunit} \tab Time
  length of a single gas pulse.  The unity value means
  experiments with steady-state sensor signals. The default
  value is \code{1}. \cr \code{gases} \tab Gas indices. \cr
  \code{ngases} \tab The number of gases. \cr \code{gnames}
  \tab Names of gases. \cr \code{concUnits} \tab
  Concentration units external to the model, values given
  in an input concentration matrix. \cr \code{concUnitsInt}
  \tab Concentration units internal to the model. \cr
  \code{df} \tab Data frame of a concentration profile with
  the columns: names of gases (\code{gnames}), \code{time},
  \code{set}, \code{lab} and \code{tpoint}. \cr }

  Methods of the class: \tabular{rl}{ \code{add<-} \tab A
  replacement method to add gas pulses to the object. \cr
  \code{getConc} \tab Get the concentration matrix of the
  object. \cr }

  The \code{plot} method has one type (parameter \code{y}):
  \tabular{rl}{ \code{time} \tab Shows the concentration of
  gases over time. \cr }
}
