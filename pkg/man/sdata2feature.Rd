\name{sdata2feature}
\alias{sdata2feature}
\title{Method sdata2feature}
\description{
  Method sdata2feature converts a matrix of sensor data
  into a data frame of features.
}
\details{
  The input parameters are an object, e.g. of class
  \code{SensorArray}, a concentration matrix, a matrix of
  sensor data, and (optionally) a data frame derived from
  conccentraion matrix.
}
\examples{
### 1) a concentration matrix of three gases (tunit 4)
sa <- SensorArray(tunit = 4)

set <- c("A 0.1", "B 0.1", "C 1")
sc <- Scenario(set, tunit = 4)
conc <- getConc(sc)

head(conc)

sdata <- predict(sa, conc)

df <- sdata2feature(sa, conc, sdata)
head(df)
}

