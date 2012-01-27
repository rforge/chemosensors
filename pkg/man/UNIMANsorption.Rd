\docType{data}
\name{UNIMANsorption}
\alias{UNIMANsorption}
\title{Dataset UNIMANsorption.}
\description{
  The dataset contains the statistics on modeling the
  Langmuir isotherm on 17 UNIMAN sensors and 3 pure
  analytes at different concentration levels.
}
\details{
  Indeed, the isotherm extends the Langmuir isotherm for a
  single gas under a simplified assumption that molecules
  of the analytes in mixture do not interact with each
  other. Such property allows us to describe the adsorption
  process in the gas mixture explicitly by computing a
  single-adsorption Langmuir isotherm per analyte.

  We estimate the parameters of the Langmuir isotherm by
  fitting a linear model based on the short-term UNIMAN
  dataset \code{\link{UNIMANshort}}. The resulted
  coefficients of determination \code{R2} of the models are
  not below than 0.973 for analyte C, and slightly worse
  for analytes A and B giving the minimum value 0.779.

  The datasets has the only variable \code{qkc} of the
  class \code{array} of three dimensions. The first
  dimension encodes a sensor, and the second encodes a gas.
  The third dimension represent four parameters extracted
  from the Langmuir model: \tabular{rl}{ \code{K} \tab
  Sorption affinity in terms of the Langmuir isotherm. \cr
  \code{Q} \tab Sorption capacity in terms of the Langmuir
  isotherm (not used in \code{\link{SorptionModel}}). \cr
  \code{KCmin} \tab The term \code{KC} in the dominator of
  the isotherm at minimal concentration level (analyte
  contribution in a mixture). \cr \code{KCmax} \tab The
  term \code{KCmax} in the dominator of the isotherm at
  maximum concentration level (analyte contribution in a
  mixture). \cr }
}
\examples{

data(UNIMANsorption, package="chemosensors")

# print the list of loaded data variables
ls()

print(dim(qkc))

print(str(qkc))

barplot(qkc[, , "K"], beside=TRUE, main="Affinity parameter K ~ UNIMAN sensors")

barplot(qkc[, , "KCmin"], beside=TRUE, main="Parameter KCmin ~ UNIMAN sensors")

barplot(qkc[, , "KCmax"], beside=TRUE, main="Parameter KCmax ~ UNIMAN sensors")
}
\seealso{
  \code{\link{SorptionModel}}
}
\keyword{data}
\keyword{datasets}

