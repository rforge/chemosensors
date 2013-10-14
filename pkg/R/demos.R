#' Demo SensorArrayNoise.
#'
#' @name demo-SensorArrayNoise
#' @rdname demo-SensorArrayNoise
#' @docType data
#' @keywords demo
#' @example demo/SensorArrayNoise.R
NULL

#' Demo SensorAffinity.
#'
#' @name demo-SensorAffinity
#' @rdname demo-SensorAffinity
#' @docType data
#' @keywords demo
#' @example demo/SensorAffinity.R
NULL

#' Demo SensorNoise.
#'
#' @name demo-SensorNoise
#' @rdname demo-SensorNoise
#' @docType data
#' @keywords demo
#' @example demo/SensorNoise.R
NULL

#' Demo VirtualSensors.
#'
#' @name demo-VirtualSensors
#' @rdname demo-VirtualSensors
#' @docType data
#' @keywords demo
#' @example demo/VirtualSensors.R
NULL

#' Demo Mixtures.
#'
#' @name demo-Mixtures
#' @rdname demo-Mixtures
#' @docType data
#' @keywords demo
#' @example demo/Mixtures.R
NULL

#' Demo BinaryMixtures.
#'
#' @name demo-BinaryMixtures
#' @rdname demo-BinaryMixtures
#' @docType data
#' @keywords demo
#' @example demo/BinaryMixtures.R
NULL

#' Demo NeuromorphicSim.
#'
#' This demonstration shows an example of neuromorphic simulations 
#' possible with package \code{\link{chemosensors-package}}. 
#' Particularly, we perform a mixture quantification scenario,
#' where the system is trained on pure analytes and validation is thought to
#' quantify concentration of components in mixtures. 
#'
#' The user should be aware of the following issues when implementing such simulations.
#' \itemize{
#'   \item{A concentration matrix (of three analytes maximum) can be coded in several ways,
#'     manually by function \code{matrix} or by using methods of class \code{Scenario}.
#'     See the example code on class help page \code{\link{Scenario}} for more details}.
#'   \item{To configure a sensor array, there are a list of parameters for initialization method
#'     of class \code{\link{SensorArray}}: the number of sensors (\code{nsensors}), 
#'     sensor types (\code{num}), sensor non-linearity (\code{alpha}),
#'     sensor diversity (\code{beta}), noise levels (\code{csd}, \code{ssd} and \code{dsd}),
#'     and sensor dynamics (\code{enableDyn}).}
#'   \item{To parallelize the computation in data generation process, one can specify the parameter
#'   \code{nclusters} (default value is \code{1}) of method \code{predict}.
#'   Another alternative is to set a global option \code{cores} by command \code{options(cores = 2)}.}
#' }
#' 
#' @name demo-NeuromorphicSim
#' @rdname demo-NeuromorphicSim
#' @docType data
#' @keywords demo
#' @example demo/NeuromorphicSim.R
NULL
