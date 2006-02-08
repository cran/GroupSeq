\name{computeAlphaLevel}

\alias{computeAlphaLevel}

\title{Computes alpha level and other probabilities}

\description{
Computes alpha level and other probabilities for the number of
tests of hypotheses.  Method is a modification of Armitage,
McPherson and Rowe.  Consistent with Lan & DeMets 83, does not
assume equally spaced information times.
}

\usage{
computeAlphaLevel(n, t, t2, lowerBounds, upperBounds, drift, nMax)
}

\arguments{
  \item{n}{the number of analyses}
  \item{t}{the vector of analysis time point on (0,1).}
  \item{t2}{is the second or information time scale, for covariances}
  \item{lowerBounds}{the vector of upper bounds (standardized)}
  \item{upperBounds}{the vector of lower bounds (standardized)}
  \item{drift}{the noncentrality; at t=1, process has mean drift.}
  \item{nMax}{Number of interim analyses is limited to \code{nMax=25}.}
}

\value{
  \item{probStopping(i)}{is the prob of reaching ith analysis and stopping.}
  \item{probExceedingUpper(i)}{is the prob of reaching ith and exceeding upper.}
  \item{probExceedingLower(i)}{is the prob of reaching ith and exceeding lower.}
  \item{expectedStoppingTime}{is expected stopping time (not implemented).}
  \item{probTotal}{is the total type I error used.}
}

\references{ Armitage, McPherson & Rowe 1969 JRSSA \cr
             Lan & DeMets 1983 Biometrika \url{http://www.biostat.wisc.edu/landemets/} }

\author{ Roman Pahl }

\note{This function is meant to be called in context of using \code{\link{groupseq}}}

\seealso{ \code{\link{groupseq}}, \code{\link{stdDeviations}} 
          \code{\link{pnorm}}, \code{\link{jointDensity}}}

\examples{
## fictious example 
computeAlphaLevel(3, c(0.33,0.66,1),c(0.33,0.66,1), c(-1,0,1), c(2,3,4),0.2, 25)
## with following result:
#  $probAndStop
# [1] 0.1621561 1.0000000 1.0000000
#
# $probAndExceedingUpper
# [1] 2.970758e-02 7.447682e-04 9.675729e-06
#
# $probAndExceedingLower
# [1] 0.1324485 0.3115237 0.3327052
#
# $expectedStoppingTime
# [1] 0
#
# $totalTypeOneError
# [1] 0.8071395
#
}

\keyword{methods}

