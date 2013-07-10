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
  The datasets has one variable \code{UNIMANsnoise} of the
  class \code{list} to store another list of coefficients
  \code{Bsd}. The sd values themselves are stored in a
  matrix of 3 rows and 17 columns under two categories:

  \itemize{ \item{The class name: \code{SensorModel} and
  \code{Sensor}.} \item{The model name: \code{plsr},
  \code{mvr}, \code{broken-stick} and \code{plsr}.} }

  Thus, in order to access to the sd coefficients of 17
  UNIMAN sensors for class \code{Sensor} and model
  \code{plsr}, the command looks like
  \code{UNIMANsnoise$Bsd$Sensor$plsr}.

  Notes.

  \itemize{ \item{A possible way to compare the sd
  coefficients (which UNIMAN sensors are more noisy) is to
  normalize them across gases and compare the resulted
  normalized values (see Example section).  Indeed, it is
  not absolutely fair, as the sensitivity coefficient
  values (sd values are derived from) are different along
  sensors, and larger values tend to show larger sd.} }
}
