\name{Sensor}
\alias{Sensor}
\title{Wrapper to Class Sensor.}
\usage{
  Sensor(num = 1, ...)
}
\description{
  Wrapper to Class Sensor.
}
\examples{
# sensor object: default initialization
s <- Sensor()

# get information about the sensor
show(s)
print(s)

plot(s)  

# sensor object: custom parameters
s <- Sensor(num=5, enableSorption=FALSE) # sorption model disabled
plot(s, main="Sensor with sorption model disabled")

s <- Sensor(num=5, Knonlin=0.01, gases=c(1, 3)) # amost linear sensor
plot(s, main="Almost linear sensor, non-linearity 0.01")

s <- Sensor(num=5, Knonlin=1, gases=c(1, 3)) # saturated sensor
plot(s, main="Saturated sensor, non-linearity 1")

s <- Sensor(num=5, csd=0, ssd=0) # noise level is set to zero
plot(s, "noise", main="Noise-free sensor")

s <- Sensor(num=5, csd=1, ssd=1) # maximum reasonable level of noise
plot(s, "noise", main="Very noisy sensor")

# method plot
#  - plot types 'y': response, noise
s <- Sensor() # default model

plot(s, "response", main="plot(s, 'response')") # default plot type, i.e. 'plot(s)' does the same plotting

plot(s, "noise", main="plot(s, 'noise')")

}
\seealso{
  \code{\link{SensorArray}}, \code{\link{SensorModel}}
}
\keyword{Sensor}

