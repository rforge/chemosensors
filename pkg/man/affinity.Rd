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
plot(s, type="inc", affinity=TRUE, main="Sensor: num 1, affinity (C) > affinity (A), affinity(B) = 0")

nsensors <- c(17, 50, 100)
for(n in nsensors) {
  sa <- SensorArray(nsensors=n, gases=c(1, 3))
  plot(sa, 'affinity', main=paste("Affinity Map: nsensors", n))
}
}
\seealso{
  \code{\link{SensorArray}}
}

