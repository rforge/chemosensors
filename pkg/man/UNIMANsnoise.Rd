\docType{data}
\name{UNIMANsnoise}
\alias{UNIMANsnoise}
\title{Dataset UNIMANsnoise.}
\description{
  The dataset contains the statistics on degradation in the
  individual performance of UNIMAN sensor in terms of
  standard deviation of sensitivity coefficients computed
  over the long-term UNIMAN dataset.
}
\details{
  The datasets has one variable \code{Bsd} of the class
  \code{list}. The sd values themselves are stored in a
  matrix of 3 rows and 17 columns under two categories:
  \tabular{rl}{ \code{class} \tab The class name:
  \code{SensorModel} and \code{Sensor}. \cr \code{model}
  \tab The model name: \code{mvr} and \code{plsr}. \cr }
}
\examples{

data(UNIMANsnoise, package="chemosensors")

# print the list of loaded data variables
ls()

print(str(Bsd))

# SD values for beta-coefficients in the case of 'SensorModel/mvr'
bsd <- Bsd[["SensorModel"]][["mvr"]]

print(dim(bsd)) # 3 gases, 17 sensors

print(apply(bsd, 1, summary)) # summary per gas

barplot(bsd, names.arg=1:ncol(bsd), main="SD of coefficients ~ Sensors")
}
\seealso{
  \code{\link{SensorNoiseClass}}
}
\keyword{data}
\keyword{datasets}

