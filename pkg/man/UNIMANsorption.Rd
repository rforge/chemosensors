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

  The datasets has the only variable \code{UNIMANsorption}
  of class list, that in turn stores the variable
  \code{qkc} of the class \code{array} of three dimensions.
  The first dimension encodes a sensor, and the second
  encodes a gas. The third dimension represent four
  parameters extracted from the Langmuir model:

  \tabular{rl}{ \code{K} \tab Sorption affinity in terms of
  the Langmuir isotherm. \cr \code{Q} \tab Sorption
  capacity in terms of the Langmuir isotherm (not used in
  \code{\link{SorptionModel}}). \cr \code{KCmin} \tab The
  term \code{KC} in the dominator of the isotherm at
  minimal concentration level (analyte contribution in a
  mixture). \cr \code{KCmax} \tab The term \code{KCmax} in
  the dominator of the isotherm at maximum concentration
  level (analyte contribution in a mixture). \cr }
}
\examples{

data(UNIMANsorption, package="chemosensors")

# print the list of loaded data variables
str(UNIMANsorption)

dim(UNIMANsorption$qkc)

str(UNIMANsorption$qkc)

### Langmuir parameter K
K <- UNIMANsorption$qkc[, , "K"]

mf <- melt(K, varnames = c("sensor", "gas"))

p1 <- qplot(sensor, value, data = mf, geom = "line", color = gas) +
  ylab("Langmuir parameter K")
p1

p2 <- qplot(sensor, value, data = mf, geom = "bar", stat = "identity") + 
  facet_grid(gas ~ ., scale = "free_y") + 
  ylab("Langmuir parameter K")
p2

### Langmuir parameter KCmin
KCmin <- UNIMANsorption$qkc[, , "KCmin"]

mf <- melt(KCmin, varnames = c("sensor", "gas"))

p3 <- qplot(sensor, value, data = mf, geom = "line", color = gas) +
  ylab("Langmuir parameter KCmin")
p3

p4 <- qplot(sensor, value, data = mf, geom = "bar", stat = "identity") + 
  facet_grid(gas ~ .) + 
  ylab("Langmuir parameter KCmin")
p4

### Langmuir parameter KCmax
KCmax <- UNIMANsorption$qkc[, , "KCmax"]

mf <- melt(KCmax, varnames = c("sensor", "gas"))

p5 <- qplot(sensor, value, data = mf, geom = "line", color = gas) +
  ylab("Langmuir parameter KCmax")
p5

p6 <- qplot(sensor, value, data = mf, geom = "bar", stat = "identity") + 
  facet_grid(gas ~ .) + 
  ylab("Langmuir parameter KCmax")
p6

### summary plot for K*
require(gridExtra)
grid.arrange(p1, p3, p5, ncol = 1)

### plot to group sensors based on affinities A vs. C
df <- as.data.frame(K)
df <- mutate(df,
  sensor = 1:nrow(df),
  sensor.group = ifelse(A > C, "More affinity to A", "More affinity to C"))

mf <- melt(K, varnames = c("sensor", "gas"))

p7 <- ggplot(mf, aes(x = factor(sensor), y = value, fill = gas)) + 
  geom_bar(position = "dodge") +
  xlab("sensor") + ylab("Langmuir parameter K")
p7

p8 <- ggplot(df, aes(reorder(x = factor(sensor), A - C), y = A - C, fill = sensor.group)) + 
  geom_bar(position = "identity") + coord_flip() +
  xlab("sensor") + ylab("Difference in K between A and C")
p8

### UNIMAN affinities K in polar plot
mf <- melt(UNIMANsorption$qkc[, , "K"], varnames = c("sensor", "gas"))

p9 <- qplot(sensor, value, color = gas, data = mf, geom = "line") + coord_polar()
p9


}
\seealso{
  \code{\link{SorptionModel}}
}
\keyword{data}
\keyword{datasets}

