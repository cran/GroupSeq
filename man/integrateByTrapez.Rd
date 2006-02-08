\name{integrateByTrapez}

\alias{integrateByTrapez}

\title{ Numerical integration }
\description{
Function does Numerical integration by composite trapezoidal rule.
}

\usage{
integrateByTrapez(functionValues, numberOfIntegrationIntervalls, gridSize)
}

\arguments{
  \item{functionValues}{ is a vector of function values. }
  \item{numberOfIntegrationIntervalls}{ is the number of trapezoids with base gridSize respective
                                        the number of intervals for this numerical integration.}
  \item{gridSize}{ is the grid size }
}

\value{
  \item{area }{is the sum of the specified function values.}
}

\references{ This refers to Fortran Implementation from \url{http://www.biostat.wisc.edu/landemets/}  }

\author{ Roman Pahl }

\note{
This function is meant to be called in context of using \code{\link{groupseq}}
}

\seealso{ \code{\link{groupseq}} }

\examples{
## function depends heavily on calculations before (e.g.the 'grid'), 
## so you don´t want to use it by manual and therefore an example is not useful
}

\keyword{methods}
