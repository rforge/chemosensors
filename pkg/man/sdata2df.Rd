\name{sdata2df}
\alias{sdata2df}
\title{Method sdata2df}
\description{
  Method sdata2df converts a matrix of sensor data into a
  data frame.
}
\details{
  The input parameters are an object, e.g. of class
  \code{SensorArray}, and a matrix of sensor data.
}
\examples{

### 1) a concentration matrix of three gases (tunit 4)
sa <- SensorArray(tunit = 4)

set <- c("A 0.1", "B 0.1", "C 1")
sc <- Scenario(set, tunit = 4)
conc <- getConc(sc)

head(conc)

sdata <- predict(sa, conc)

sf <- sdata2df(sa, sdata)

head(sf)
}

