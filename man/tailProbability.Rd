\name{tailProbability}

\alias{tailProbability}

\title{ tail probability }

\description{
This function calculates tail probability by numerical integration.
}
\usage{
tailProbability(upperBound, previousDensity, numberOfIntegrationIntervalls, lowerIntegrationLimit, upperIntegrationLimit, standardDeviation)
}

\arguments{
  \item{upperBound}{ ~is the argument for evaluation.}
  \item{previousDensity}{ is the vector of density values from the previous step.}
  \item{numberOfIntegrationIntervalls}{ is the number of intervals for numerical integration. }
  \item{lowerIntegrationLimit}{ is the lower integration limit from the previous step. }
  \item{upperIntegrationLimit}{ is the upper integration limit from the previous step. }
  \item{standardDeviation}{ is the standard deviation of the process increment. }
}

\value{
  \item{result }{which is the calculated tail probability}
}

\references{ \url{http://www.biostat.wisc.edu/landemets/} }

\author{ Roman Pahl }

\note{
This function is meant to be called in context of using \code{\link{groupseq}}
}

\seealso{ \code{\link{groupseq}}, \code{\link{pnorm}} 
          \code{\link{integrateByTrapez}} }

\examples{
## function depends heavily on calculations before (e.g. 'previousDensity') and calls himself several times
## so you don´t want to use it by manual and therefore an example is not useful.
}

\keyword{methods}

