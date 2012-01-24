\docType{package}
\name{chemosensors}
\alias{chemosensors}
\alias{chemosensors-package}
\title{Package chemosensors.}
\description{
  A tool to set up synthetic experiments in machine
  olfaction.
}
\examples{

# concentration matrix of 3 gas classes: A, C and AC
conc <- matrix(0, 300, 3)
conc[1:100, 1] <- 0.05 #  A
conc[101:200, 3] <- 1 # C
conc[201:300, 1] <- 0.05 # AC
conc[201:300, 3] <- 1 # AC

conc <- conc[sample(1:nrow(conc)  ), ]

# sensor array of 5 sensors with parametrized noise parameters
sa <- SensorArray(num=1:5, csd=0.1, ssd=0.1, dsd=0.1)

# get information about the array
print(sa)
plot(sa)

# generate the data
sdata <- predict(sa, conc)

# plot the data
plot(sa, "prediction", conc=conc)
}

