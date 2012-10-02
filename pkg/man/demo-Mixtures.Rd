\docType{data}
\name{demo-Mixtures}
\alias{demo-Mixtures}
\title{Demo Mixtures.}
\description{
  Demo Mixtures.
}
\examples{

### experiment #1: all type of mixtures
#  - three pure analytes
#  - three binary mixtures
#  - one ternary mixture
# NOTE: data model of type 'mvr' is prefered

n <- 10
sc <- Scenario()
add(sc) <- list("A", 0.05, n)
add(sc) <- list("B", 0.05, n)
add(sc) <- list("C", 1, n)
add(sc) <- list(c("A", "B"), c(0.05, 0.05), n)
add(sc) <- list(c("A", "C"), c(0.05, 1), n)
add(sc) <- list(c("B", "C"), c(0.05, 1), n)
add(sc) <- list(c("A", "B", "C"), c(0.05, 0.05, 0.5), n)

conc <- getConc(sc)

sa <- SensorArray(model="mvr", num=1:5, csd=0.1, ssd=0.1, dsd=0)
sdata <- predict(sa, conc) # generate the data
plot(sa, 'pca', conc=conc, sdata=sdata, leg="top") # plot

### experiment #2: analytes A and C + their binary mixture

n <- 10
sc <- Scenario()
add(sc) <- list("A", 0.01, n)
add(sc) <- list("A", 0.05, n)
add(sc) <- list("C", 0.1, n)
add(sc) <- list("C", 1, n)
add(sc) <- list(c("A", "C"), c(0.01, 0.1), n)
add(sc) <- list(c("A", "C"), c(0.05, 1), n)

conc <- getConc(sc)

sa <- SensorArray(num=1:5, csd=0.1, ssd=0.1, dsd=0) # default data model: 'ispline'
sdata <- predict(sa, conc) # generate the data
plotPCA(sa, conc=conc, sdata=sdata, leg="top") # plot
}
\keyword{datasets}
\keyword{demo}

