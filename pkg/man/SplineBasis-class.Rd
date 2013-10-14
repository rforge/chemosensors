\name{SplineBasis-class}
\alias{SplineBasis}
\alias{SplineBasis-class}
\alias{fitSplineBasis}
\alias{getSplineDesign}
\alias{splineBasis}
\title{SplineBasis class.}
\usage{
  splineBasis(type = "bspline", x, breaks, rangeval,
    norder = 3, lambda = 0.1, tol = 1e-16, first = 0,
    last = 0)

  SplineBasis(...)

  getSplineDesign(bs, x, all = TRUE)

  fitSplineBasis(bs, x, y)
}
\arguments{
  \item{type}{Type of spline basis: 'bspline', 'mspline' or
  'ispline'. The default value is 'bspline'.}

  \item{x}{Numeric vector of data points.}

  \item{breaks}{Break points. Include the boundary knots;
  ordered.}

  \item{rangeval}{The range of data points (minimum and
  maximum values). Must include the break points.}

  \item{norder}{The order of the basis functions. The cubic
  spline basis has the order 4. The default value: 3.}

  \item{lambda}{Smoothing parameter for quadratic solver
  when performing fitting with restriction conditions.}

  \item{tol}{Tolerance value for stopping criteria in
  iterative procedures.}

  \item{first}{The number of first basis functions to be
  omitted. Supported values: 0 or 1. The default value is
  0.}

  \item{last}{The number of last basis functions to be
  omitted. Supported values: 0 or 1. The default value is
  0.}

  \item{...}{Parameters of constructor.}

  \item{all}{Logical whther include all basis functions.}

  \item{bs}{An object of class \code{SplineBasis}.}

  \item{y}{Numeric vector of data points (respone).}
}
\value{
  An object of class 'SplineBasis'.

  A design matrix.

  Numeric vector of beta coefficients.
}
\description{
  Slots: type, rangeval, nbasis, params, breaks, norder,
  knots.

  Constructor of SplineBasis class.

  Wrapper to constructor of SplineBasis Class.

  Function getSplineDesign.

  Function fitSplineBasis.
}
\details{
  The number of spline basis functions: \eqn{nbasis =
  norder + nknots - 2}.
}

