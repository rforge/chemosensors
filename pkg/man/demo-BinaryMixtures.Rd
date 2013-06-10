\docType{data}
\name{demo-BinaryMixtures}
\alias{demo-BinaryMixtures}
\title{Demo BinaryMixtures.}
\description{
  Demo BinaryMixtures.
}
\examples{

s <- Sensor(num=1, gases=c(1, 3), enableSorption=FALSE)
plotMixture(s)

s <- Sensor(num=1, gases=c(1, 3))
plotMixture(s)

opar <- par(mfrow=c(2, 2))
for(num in c(1, 4, 13, 17)) {
  s <- Sensor(num=num, gases=c(1, 3))
  Ka <- round(affinity(s), 0)
  K <- round(coef(s, 'SorptionModel'), 0)
  plotMixture(s, axes=FALSE,
    xlab = paste("Gas A, K", K[1], "( affinity", Ka[1], ")"),
    ylab = paste("Gas C, K", K[2], "( affinity", Ka[2], ")"),    
    main=paste("num", num))
}
par(opar)
}
\keyword{datasets}
\keyword{demo}

