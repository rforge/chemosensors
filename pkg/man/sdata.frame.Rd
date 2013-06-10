\name{sdata.frame}
\alias{sdata.frame}
\title{Method sdata.frame}
\description{
  Method sdata.frame converts a concetration matrix and
  (optionally) a sensor data matrix into a data frame.
}
\details{
  The input parameters are an object, e.g.
  \code{SensorArray}, a concentration matrix, and
  (optionally) a sensor data matrix. The output data frame
  has the following columns:

  \tabular{rl}{ \code{S1}, \code{S2}, ... \tab Sensor
  signals. \cr \code{A}, \code{B}, ... \tab Gas
  concentrations (column names equal to gas names of the
  object). \cr \code{glab} \tab Gas labels, e.g. \code{A}
  or \code{Air}. \cr \code{lab} \tab Gas+Concetratoin
  labels, e.g. \code{A 0.01}. \cr \code{tpoint} \tab Time
  point labels to encode the gas pulses, e.g. \code{gasin}.
  }
}
\examples{

### 1) a concentration matrix of three gases (tunit 4)
sa <- SensorArray(tunit = 4)

set <- c("A 0.1", "B 0.1", "C 1")
sc <- Scenario(set, tunit = 4)
conc <- getConc(sc)

head(conc)

sdata <- predict(sa, conc)

p1 <- plotSignal(sa, conc = conc, sdata = sdata)
p1

# get a data.frame of features
df.transient <- sdata.frame(sa, conc, sdata, feature = "transient")

df.ss <- sdata.frame(sa, conc, sdata, feature = "steady-state")

df.step <- sdata.frame(sa, conc, sdata, feature = "step")

# plot
p2 <- ggplot(df.transient, aes(x = 1:length(S1))) +
  geom_line(aes(y = S1, color = "S1")) +
  geom_line(aes(y = S2, color = "S2")) +
  labs(title = "feature: transient")
p2

df <- rbind(data.frame(df.ss, feature = "steady-state"),
  data.frame(df.step, feature = "step"))
  
p3 <- ggplot(df, aes(lab, S1, fill = feature)) + geom_bar(position = "dodge")
p3
}

