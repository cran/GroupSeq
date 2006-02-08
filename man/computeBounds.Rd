\name{computeBounds}

\alias{computeBounds}


\title{calculate boundaries}

\description{
Function to calculate boundaries corresponding to
particular type I error spending rates and times of
interim analyses, see Lan & DeMets 83.
}

\usage{
computeBounds(n, drift, alpha, phi, t, t2, OneOrTwoSidedBounds, whatSpendingFunctionIsUsed, boundsTruncation)
}

\arguments{
  \item{n}{ is the number of interim analyses }
  \item{drift}{ drift is the true mean of the process, the noncentrality - drift is set to zero, and so does not affect function }
  \item{alpha}{is the total type I error.}
  \item{phi}{ optional Parameter referring to Power Family}
  \item{t}{is the information times.}
  \item{t2}{ is the 'second' time scale, used for the covariance of sequential statistics, usually number of events or subjects. }
  \item{OneOrTwoSidedBounds}{ indicates one or two sided boundaries. }
  \item{whatSpendingFunctionIsUsed}{ indicates type I error spending function. }
  \item{boundsTruncation}{ is the user selected truncation on integration limits. }
}

\value{
  \item{lowerBounds }{the vector of lower standardized boundaries}
  \item{upperBounds }{the vector of upper standardized boundaries}
  \item{probExit }{vector of exit probabilities}
  \item{probDifference(i) }{probExit(i)-probExit(i-1)}
}

\references{Lan & DeMets 1983 Biometrika \url{http://www.biostat.wisc.edu/landemets/}}

\author{Roman Pahl}

\note{ drift MAY NOT be changed without adding code! \cr
This function is meant to be called in context of using \code{\link{groupseq}}
}

\seealso{ \code{\link{groupseq}}, \code{\link{alphaByUseFunction}} 
          \code{\link{stdDeviations}}, \code{\link{pnorm}}
          \code{\link{jointDensity}}, \code{\link{tailProbability}},
          \code{\link{searchForBound}}}

\examples{
## fictious example
computeBounds(2,0,0.05,0,c(0.5,1.0),c(0.5,1.0),1,1,8)
##with following result:
#$lowerBounds
#[1] -8 -8
#
#$upperBounds
#[1] 2.537988 1.662070
#
#$exitProbabilities
#[1] 0.005574597 0.050000000
#
#$differencesExitProbabilities
#[1] 0.005574597 0.044425403
}

\keyword{methods}
\keyword{distribution}

