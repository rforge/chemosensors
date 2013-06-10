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
\examples{

### 1) a concentration matrix of three gases (tunit 1)
sa <- SensorArray() 

conc.values <- concMax(sa)
conc <- diag(conc.values)

conc 

cf <- conc2df(sa, conc)
cf

### 2) a concentration matrix of three gases (tunit = 4)
sa <- SensorArray(tunit = 4)

set <- c("A 0.1", "B 0.1", "C 1")
sc <- Scenario(set, tunit = 4)
conc <- getConc(sc)

head(conc)

cf <- conc2df(sa, conc)
head(cf)

}

