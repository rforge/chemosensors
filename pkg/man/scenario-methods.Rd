\name{scenario-methods}
\alias{add<-}
\alias{add<-,Scenario-method}
\alias{checkConc}
\alias{checkConc,ANY-method}
\alias{cmatrix}
\alias{cmatrix,Scenario-method}
\alias{conc2col}
\alias{conc2col,ANY-method}
\alias{conc2df}
\alias{conc2df,ANY-method}
\alias{conc2glab}
\alias{conc2glab,ANY-method}
\alias{conc2lab}
\alias{conc2lab,ANY-method}
\alias{conc2tpoint}
\alias{conc2tpoint,ANY-method}
\alias{getTPoint}
\alias{getTPoint,ANY-method}
\alias{lab2df}
\alias{lab2df,Scenario-method}
\alias{label2df}
\alias{label2df,Scenario-method}
\alias{nsamples}
\alias{nsamples,Scenario-method}
\alias{scenario-methods}
\alias{sdata.frame}
\alias{sdata.frame,ANY-method}
\alias{sdata.frame,Scenario-method}
\alias{sdata2df}
\alias{sdata2df,ANY-method}
\alias{sdata2feature}
\alias{sdata2feature,ANY-method}
\alias{sdata2pulse}
\alias{sdata2pulse,ANY-method}
\alias{set2lab}
\alias{set2lab,Scenario-method}
\title{Methods to control the scenario.}
\description{
  Methods to control the scenario.

  Method sdata.frame.

  Method nsamples.

  Method cmatrix.

  Method add<-.

  Method checkConc.

  Method sdata2feature.

  Method sdata2df.

  Method conc2df.

  Method conc2lab.

  Method conc2glab.

  Method conc2col.

  Method getTPoint.

  Method conc2tpoint.

  Method lab2df.

  Method label2df.

  Method set2lab.

  Method sdata.frame converts a concetration matrix and
  (optionally) a sensor data matrix into a data frame.

  Method conc2df converts a concetration matrix into a data
  frame.

  Method sdata2feature converts a matrix of sensor data
  into a data frame of features.

  Method sdata2df converts a matrix of sensor data into a
  data frame.
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

  The input parameters are an object, e.g. of class
  \code{SensorArray}, a concentration matrix, a matrix of
  sensor data, and (optionally) a data frame derived from
  conccentraion matrix.

  The input parameters are an object, e.g. of class
  \code{SensorArray}, and a matrix of sensor data.
}
\examples{
set.seed(1)

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
df.transient <- sdata.frame(sa, conc = conc, sdata = sdata, feature = "transient")

df.ss <- sdata.frame(sa, conc = conc, sdata = sdata, feature = "steady-state")

df.step <- sdata.frame(sa, conc = conc, sdata = sdata, feature = "step")

# plot
p2 <- ggplot(df.transient, aes(x = 1:length(S1))) +
  geom_line(aes(y = S1, color = "S1")) +
  geom_line(aes(y = S2, color = "S2")) +
  labs(title = "feature: transient")
p2

df <- rbind(data.frame(df.ss, feature = "steady-state"),
  data.frame(df.step, feature = "step"))
  
p3 <- ggplot(df, aes(lab, S1, fill = feature)) + geom_bar(stat = "identity", position = "dodge")
p3

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

### 1) a concentration matrix of three gases (tunit 4)
sa <- SensorArray(tunit = 4)

set <- c("A 0.1", "B 0.1", "C 1")
sc <- Scenario(set, tunit = 4)
conc <- getConc(sc)

head(conc)

sdata <- predict(sa, conc)

df <- sdata2feature(sa, conc, sdata)
head(df)

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

