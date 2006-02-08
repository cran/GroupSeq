\name{findDrift}

\alias{findDrift}

\title{ Find drift resulting in power "confidenceLevel". }

\description{
The drift will be that value producing exit probability "confidenceLevel"
at time t(n) given the bounds.  If t(n) < 1, this is not the 
power of the study.
}

\usage{
findDrift(n, t, t2, lowerBounds, upperBounds, confidenceLevel, drift, nMax)
}

\arguments{
  \item{n}{ the number of analyses. }
  \item{t}{ the vector of analysis time point on (0,1). }
  \item{t2}{ is the second or information time scale, for covariances}
  \item{lowerBounds}{the vector of lower bounds (standardized). }
  \item{upperBounds}{ the vector of upper bounds (standardized). }
  \item{confidenceLevel}{ desired power respectively confidence level }
  \item{drift}{  vector of drift parameters }
  \item{nMax}{ Number of interim analyses is by default limited to \code{nMax=25} }
}
\details{
This function directly calls \code{\link{computeDrift}}
}

\value{
  \item{drift }{computed drift}
}

\author{ Roman Pahl }

\note{This function is meant to be called in context of using \code{\link{groupseq}}}

\seealso{ 
\code{\link{groupseq}}, \code{\link{computeDrift}} 
}

\examples{
## fictious example
computeDrift(2, c(0.5,1.0), c(0.5,1.0), c(-8,-8), c(2.3,1.1), 0.3, 0, 25)
## with following result:
## [1] 0.5696356
}

\keyword{methods} 