\name{Scenario-class}
\alias{Scenario}
\alias{Scenario-class}
\alias{defaultParScenario}
\title{Class Scenario.}
\usage{
  defaultParScenario()

  Scenario(...)
}
\arguments{
  \item{...}{parameters of constructor.}
}
\value{
  List of the default parameters.
}
\description{
  Class \code{Scenario} represents a concentration profile
  to build a concentration matrix.

  Function to get default constructor parameters of class
  \code{\link{Scenario}}.

  Constructor method of Scenario Class.

  Wrapper function Scenario.
}
\details{
  Slots of the class: \tabular{rl}{ \code{tunit} \tab Time
  length of a single gas pulse.  The unity value means
  experiments with steady-state sensor signals. The default
  value is \code{1}. \cr \code{gases} \tab Gas indices. \cr
  \code{ngases} \tab The number of gases. \cr \code{gnames}
  \tab Names of gases. \cr \code{concUnits} \tab
  Concentration units external to the model, values given
  in an input concentration matrix. \cr \code{concUnitsInt}
  \tab Concentration units internal to the model. \cr
  \code{df} \tab Data frame of a concentration profile with
  the columns: names of gases (\code{gnames}), \code{time},
  \code{set}, \code{lab} and \code{tpoint}. \cr }

  Methods of the class: \tabular{rl}{ \code{add<-} \tab A
  replacement method to add gas pulses to the object. \cr
  \code{getConc} \tab Get the concentration matrix of the
  object. \cr }

  The \code{plot} method has one type (parameter \code{y}):
  \tabular{rl}{ \code{time} \tab Shows the concentration of
  gases over time. \cr }
}
\examples{
# scenario object (empty): default initialization 
sc <- Scenario(tunit = 60)

# add data to 
add(sc) <- list("A", 0.05, 2)
add(sc) <- list("B", 0.05, 2)
add(sc) <- list("C", 1, 2)
add(sc) <- list(c("A", "B", "C"), c(0.02, 0.04, 0.5), 2)

# scenario object: custom initialization
set <- c("A 0.05", "B 0.05", "C 1", "A 0.02, B 0.04, C 0.5") 
sc <- Scenario(tunit = 60, T = set, nT = 2, V = set, nV = 2)

# a real example of Scenario
sc <- Scenario(name = "Regression", tunit = 60, concUnits = "perc",
  T = c("A 0.01", "A 0.02", "A 0.05"), nT = 30,
  V = c("A 0.01", "A 0.02", "A 0.05"), nV = 30,
  randomize = TRUE)
sc

add(sc) <- list("A", 0.03, 30, "T")
add(sc) <- list("A", 0.04, 30, "T")
sc

# get information about the sensor
show(sc)
print(sc)

## Not run:
plot(sc)  
plot(sc, facet = FALSE, concUnits = 'norm')
## End(Not run)

# extract conc. matrix from scenatio
conc <- getConc(sc)

print(head(conc))
}

