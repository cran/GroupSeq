\name{jointDensity}

\alias{jointDensity}

\title{ calculates the join density }

\description{
This function calculates the join density 
at eather first analysis ( i==1)
or at second analysis and later ( i > 1)
}

\usage{
jointDensity(i, lowerIntegrationLimit, upperIntegrationLimit, standardDeviation, numberOfIntegrationIntervalls, lastGrid)
}

\arguments{
  \item{i}{ denotes the i-th interim analysis }
  \item{lowerIntegrationLimit}{ is the lower integration limit for the first analysis. }
  \item{upperIntegrationLimit}{ is the upper integration limit for the first analysis. }
  \item{standardDeviation}{ is the standard deviation of the process at first analysis. }
  \item{numberOfIntegrationIntervalls}{ is the number of whole steps between lowerIntegrationLimit and upperIntegrationLimit. }
  \item{lastGrid}{ is the vector of function values over the grid. }
}

\value{
  \item{lastCopy }{a temporary vector of function values because 'lastGrid' from
                   previous step itself is needed to compute each current value.}
}

\references{ This refers to Fortran Implementation from \url{http://www.biostat.wisc.edu/landemets/}  }

\author{ Roman Pahl }

\note{
This function is meant to be called in context of using \code{\link{groupseq}}
}

\seealso{ \code{\link{groupseq}}, \code{\link{dnorm}} 
          \code{\link{integrateByTrapez}} }

\examples{
## function depends heavily on calculations before (e.g.the 'numberOfIntegrationIntervalls' or 'lastGrid'), 
## so you don´t want to use it by manual and therefore an example is not useful
}

\keyword{methods}
