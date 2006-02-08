\name{computeConfidenceIntervall}

\alias{computeConfidenceIntervall}

\title{Computes confidence intervals}

\description{
Computes confidence intervals, see Kim & DeMets 87.
Given n interim analyses, bounds for the first n-1 and
the standardized statistic for the nth, the function finds
drift parameters satisfying confidence limit equations.
}

\usage{
computeConfidenceIntervall(confidenceLevel, Zvalue, n, t, t2, lowerBounds, upperBounds, nMax)
}

\arguments{
  \item{confidenceLevel}{is confidence level.}
  \item{Zvalue}{ is the Z statistic at the most recent analysis. }
  \item{n}{ is the number of analyses. }
  \item{t}{ is the vector of analysis time point on (0,1).}
  \item{t2}{ is the second or information time scale, for covariances }
  \item{lowerBounds}{ is the vector of lower bounds (standardized). }
  \item{upperBounds}{ is the vector of upper bounds (standardized). }
  \item{nMax}{maximum Number of interim analyses (default: nMax=25) }
}

\value{
  \item{confidenceLimit }{ the calculated confidence interval}
}

\references{ Kim & DeMets 1987 Biometrics \url{http://www.biostat.wisc.edu/landemets/}}

\author{ Roman Pahl }

\note{This function is meant to be called in context of using \code{\link{groupseq}}}

\seealso{ \code{\link{groupseq}}, \code{\link{qnorm}} 
          \code{\link{computeDrift}} }

\examples{
## fictious example
computeConfidenceIntervall(0.05, 1.7, 2, c(0.5,1.0), c(0.5,1.0), c(-1,0), c(1,2), 25)
## with following result:
# [1] 1.115315 1.264942
}

\keyword{ methods }
