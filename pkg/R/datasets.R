#' Dataset UNIMANshort.
#' 
#' Short-term UNIMAN datasets of 200 samples from 17 polymeric sensors. 
#' The datasets contains two matricies:
#' \tabular{rl}{
#'   \code{C} \tab The concentration matrix of 200 rows and 3 columns encodes 
#'     the concentration profile for three gases, ammonia, propanoic acid and n-buthanol.
#'     The concentration units are given in the percentage volume (\% vol.).
#'     Ammonia has three concentration levels 0.01, 0.02 and 0.05,  propanoic acid - three levels 0.01, 0.02 and 0.05,
#'     and n-buthanol - two levels 0.1 and 1. \cr
#'   \code{dat} \tab The data matrix of 200 rows and 17 columns cotains the steady-state signals of 17 sensors
#'     in response to the concentration profle \code{C}. \cr
#' }
#'
#' The reference dataset has been measured at The University of
#' Manchester (UNIMAN). Three analytes ammonia, propanoic
#' acid and n-buthanol, at different concentration levels, were
#' measured for 10 months with an array of seventeen conducting polymer sensors.
#'
#' In modeling of the array we make the distinction between
#' short-term and long-term reference data. Two hundred samples
#' from the first 6 days are used to characterize the array assuming
#' the absence of drift. The long-term reference data (not published within the package) 
#' counts for the complete number of samples from 10 months,
#' these data were used to model the sensor noise and drift,
#' see \code{\link{UNIMANsnoise}} and \code{\link{UNIMANdnoise}} for more details.
#'
#' A pre-processing procedure on outliers removal was applied to the
#' reference data. The standard method based on the
#' squared Mahalanobis distance was used with quantile equal to
#' \code{0.975}\%.
#'
#' @name UNIMANshort
#' @rdname UNIMANshort
#' @docType data
#' @keywords data
#' @seealso \code{\link{SensorModel}}, \code{\link{SensorModel}}
#' @example inst/examples/UNIMANshort-data.R
NULL

#' Dataset UNIMANsnoise.
#' 
#' The dataset contains the statistics on degradation in the individual performance of UNIMAN sensor
#' in terms of standard deviation of sensitivity coefficients computed over the long-term UNIMAN dataset. 
#'
#' The datasets has one variable \code{UNIMANsnoise} of the class \code{list} 
#' to store another list of coefficients \code{Bsd}. The sd values themselves are stored
#' in a matrix of 3 rows and 17 columns under two categories:
#'
#' \itemize{
#'  \item{The class name: \code{SensorModel} and \code{Sensor}.}
#'  \item{The model name: \code{plsr}, \code{mvr}, \code{broken-stick} and \code{plsr}.}
#' }
#'
#' Thus, in order to access to the sd coefficients of 17 UNIMAN sensors for class \code{Sensor} and model \code{plsr},
#' the command looks like \code{UNIMANsnoise$Bsd$Sensor$plsr}.
#'
#' Notes.
#'
#' \itemize{
#'  \item{A possible way to compare the sd coefficients (which UNIMAN sensors are more noisy)
#'    is to normalize them across gases and compare the resulted normalized values (see Example section).
#'    Indeed, it is not absolutely fair, as the sensitivity coefficient values (sd values are derived from) 
#'    are different along sensors, and larger values tend to show larger sd.}
#' }
#'
#' @name UNIMANsnoise
#' @rdname UNIMANsnoise
#' @docType data
#' @keywords data
#' @seealso \code{\link{SensorNoiseModel}}
#' @example inst/examples/UNIMANsnoise-data.R
NULL

#' Dataset UNIMANdnoise.
#' 
#' The dataset contains statistics on a multi-variate drift sub-space 
#' for the long-term UNMIAN dataset of 1000 samples over 8 classes.
#'
#' The subspace was evaluated via common principal component analysis (power algorithm).
#' Only three classes (analytes at maximum concentration) are used in the computation for more accurate estimation.
#' The importance of the drift components is computed as a projected variance 
#' of sensor array data of the given three classes onto the components.
#'
#' The datasets contains four variables:
#' \tabular{rl}{
#'   \code{dspace} \tab A square \code{17 x 17} matrix of the drift subspace. Columns are drift component vectors. \cr
#'   \code{ndvar} \tab  A vector of length \code{17} with the importance of the components.\cr
#'   \code{centered} \tab  Boolean indicating whether the UNIMAN data were centered before the computation. \cr
#'   \code{scaled} \tab  Boolean indicating whether the UNIMAN data were scaled before the computation. \cr
#' }
#'
#' @name UNIMANdnoise
#' @rdname UNIMANdnoise
#' @docType data
#' @keywords data
#' @seealso \code{\link{DriftNoiseModel}}
#' @example inst/examples/UNIMANdnoise-data.R
NULL

#' Dataset UNIMANsorption.
#' 
#' The dataset contains the statistics on modeling the Langmuir isotherm 
#' on 17 UNIMAN sensors and 3 pure analytes at different concentration levels.
#'
#' Indeed, the isotherm extends the Langmuir isotherm for a single gas under
#' a simplified assumption that molecules of the analytes in mixture 
#' do not interact with each other. Such property allows us to
#' describe the adsorption process in the gas mixture explicitly by
#' computing a single-adsorption Langmuir isotherm per analyte.
#'
#' We estimate the parameters of the Langmuir isotherm by fitting a linear model 
#' based on the short-term UNIMAN dataset \code{\link{UNIMANshort}}. 
#' The resulted coefficients of determination \code{R2} of the models are not below than
#' 0.973 for analyte C, and slightly worse for analytes A and B giving the minimum value 0.779.
#'
#' The datasets has the only variable \code{UNIMANsorption} of class list,
#' that in turn stores the variable \code{qkc} of the class \code{array} of three dimensions. 
#' The first dimension encodes a sensor, and the second encodes a gas.
#' The third dimension represent four parameters extracted from the Langmuir model:
#'
#' \tabular{rl}{
#'   \code{K} \tab Sorption affinity in terms of the Langmuir isotherm. \cr
#'   \code{Q} \tab Sorption capacity in terms of the Langmuir isotherm (not used in \code{\link{SorptionModel}}). \cr
#'   \code{KCmin} \tab The term \code{KC} in the dominator of the isotherm at minimal concentration level (analyte contribution in a mixture). \cr
#'   \code{KCmax} \tab The term \code{KCmax} in the dominator of the isotherm at maximum concentration level (analyte contribution in a mixture). \cr
#' }
#'
#'
#' @name UNIMANsorption
#' @rdname UNIMANsorption
#' @docType data
#' @keywords data
#' @seealso \code{\link{SorptionModel}}
#' @example inst/examples/UNIMANsorption-data.R
NULL

#' Dataset UNIMANdistr
#' 
#' @name UNIMANdistr
#' @rdname UNIMANdistr
#' @docType data
#' @keywords data
NULL

#' Dataset UNIMANtransient
#' 
#' @name UNIMANtransient
#' @rdname UNIMANtransient
#' @docType data
#' @keywords data
NULL
