\name{conc2df}
\alias{conc2df}
\title{Method conc2df}
\description{
  Method conc2df converts a concetration matrix into a data
  frame.
}
\details{
  The input parameters are an object, e.g.
  \code{SensorArray}, and a concentration matrix. The
  output data frame has the following columns:

  \tabular{rl}{ \code{A}, \code{B}, ... \tab Gas
  concentrations (column names equal to gas names of the
  object). \cr \code{glab} \tab Gas labels, e.g. \code{A}
  or \code{Air}. \cr \code{lab} \tab Gas+Concetratoin
  labels, e.g. \code{A 0.01}. \cr \code{tpoint} \tab Time
  point labels to encode the gas pulses, e.g. \code{gasin}.
  }
}
