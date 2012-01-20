\name{SorptionModel}
\alias{SorptionModel}
\title{Class SorptionModel.}
\description{
  Class \code{\link{SorptionModel}} generates a sensor
  noise.
}
\details{
  Slots of the class: \tabular{rl}{ \code{knum} \tab Sensor
  number that encodes a UNIMAN sorption profile. \cr
  \code{sorptionModel} \tab List of model parameters 'K'
  and 'Q'. \cr }
}
\examples{

sm <- SorptionModel()
print(sm)

plot(sm, "barplot")
plot(sm, "heatmap")
plot(sm, "model")
}
\keyword{SorptionModel-class}

