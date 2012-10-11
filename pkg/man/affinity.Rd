\name{affinity}
\alias{affinity}
\title{Method affinity.}
\description{
  Method \code{affinity} evalates the affinity of sensors.
}
\details{
  The affinity coefficients are computed per analyte, and
  express strength of a given sensor in response to the
  given analyte. We use the definition of receptor/ligand
  binding affinity from biochemistry here. The analog of
  the dissociation constant \code{Kd} ic computed from the
  sensibility curve, and the affinicty coefficient is given
  by formula \code{Ka = 1/Kd}.
}
