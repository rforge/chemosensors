\name{sdata.frame}
\alias{sdata.frame}
\title{Method sdata.frame}
\description{
  Method sdata.frame converts a concetration matrix and
  (optionally) a sensor data matrix into a data frame.
}
\details{
  The input parameters are an object, e.g.
  \code{SensorArray}, a concentration matrix, and
  (optionally) a sensor data matrix. The output data frame
  has the following columns:

  \tabular{rl}{ \code{S1}, \code{S2}, ... \tab Sensor
  signals. \cr \code{A}, \code{B}, ... \tab Gas
  concentrations (column names equal to gas names of the
  object). \cr \code{glab} \tab Gas labels, e.g. \code{A}
  or \code{Air}. \cr \code{lab} \tab Gas+Concetratoin
  labels, e.g. \code{A 0.01}. \cr \code{tpoint} \tab Time
  point labels to encode the gas pulses, e.g. \code{gasin}.
  }
}
