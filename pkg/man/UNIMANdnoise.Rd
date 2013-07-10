\docType{data}
\name{UNIMANdnoise}
\alias{UNIMANdnoise}
\title{Dataset UNIMANdnoise.}
\description{
  The dataset contains statistics on a multi-variate drift
  sub-space for the long-term UNMIAN dataset of 1000
  samples over 8 classes.
}
\details{
  The subspace was evaluated via common principal component
  analysis (power algorithm). Only three classes (analytes
  at maximum concentration) are used in the computation for
  more accurate estimation. The importance of the drift
  components is computed as a projected variance of sensor
  array data of the given three classes onto the
  components.

  The datasets contains four variables: \tabular{rl}{
  \code{dspace} \tab A square \code{17 x 17} matrix of the
  drift subspace. Columns are drift component vectors. \cr
  \code{ndvar} \tab A vector of length \code{17} with the
  importance of the components.\cr \code{centered} \tab
  Boolean indicating whether the UNIMAN data were centered
  before the computation. \cr \code{scaled} \tab Boolean
  indicating whether the UNIMAN data were scaled before the
  computation. \cr }
}
