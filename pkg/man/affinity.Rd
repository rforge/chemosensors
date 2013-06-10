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
\examples{

# create a sensor
s <- Sensor(num=1)

# compute affinities
Ka <- affinity(s)
Kd <- 1 / Ka

print(Ka)
print(Kd)

# plot sensor response at conc. range of type 'inc' (see 'concSample' for more details)
#  - add lines related to computation of affinities
plot(s, Ka=Ka, type="inc", affinity=TRUE)
plot(s, Ka=Ka, type="inc", affinity=TRUE, xlim=c(0, 0.1))

#sa <- SensorArray(num=1:17, gases=c(1, 3))
#plot(sa, 'affinityMap')
}
\seealso{
  \code{\link{SensorArray}}
}

