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
\examples{

data(UNIMANsnoise)

str(UNIMANsnoise, max.level = 2)

str(UNIMANsnoise$Bsd$Sensor, max.level = 1)

# SD parameters for a particular data model 'plsr'
Bsd <- UNIMANsnoise$Bsd$Sensor$plsr

# plot #1
df <- melt(Bsd, varnames = c("gas", "sensor"))

df <- mutate(df,
  gas = LETTERS[gas], 
  sensor = factor(paste("S", sensor, sep = ""), levels = paste("S", 1:17, sep = "")))
  
p1 <- qplot(sensor, value, data = df, geom = "bar") + 
  facet_grid(gas ~ ., scales = "free_y") +
  labs(x = "sensor", y = "sd parameter", title = "Sensor Noise in data model 'plsr'")
p1  
  

# plot #2
Bsd.norm <- t(apply(Bsd, 1, function(x) x / max(x)))

df <- melt(Bsd.norm, varnames = c("gas", "sensor"))

df <- mutate(df,
  gas = LETTERS[gas], 
  sensor = factor(paste("S", sensor, sep = ""), levels = paste("S", 1:17, sep = "")))

p2 <- ggplot(df, aes(x = sensor, y = value, fill = gas)) + 
  geom_bar(position = "stack") +
  labs(x = "sensor", y = "sd parameter (normalized acroos gases)")
p2

# plot PCA plots for sensors different in the noise level
set.seed(10)
sa1 <- SensorArray(model = "plsr", num = c(4, 7, 14), csd = 0, ssd = 1, dsd = 0)

p3 <- plotPCA(sa1, set = rep(c("A", "B", "C"), 10), air = FALSE) + 
  labs(title = "Less noisy sensors")
p3

sa2 <- SensorArray(model = "plsr", num = c(1, 5, 17), csd = 0, ssd = 1, dsd = 0)

p4 <- plotPCA(sa2, set = rep(c("A", "B", "C"), 10), air = FALSE) + 
  labs(title = "More noisy sensors")
p4
}
\seealso{
  \code{\link{SensorNoiseModel}}
}
\keyword{data}
\keyword{datasets}

