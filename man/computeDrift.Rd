\name{computeDrift}

\alias{computeDrift}


\title{Computes drift}

\description{
here its original description in the Fortran-Implementation 
from \url{http://www.biostat.wisc.edu/landemets/} :

"Find drift resulting in power "conf".
The drift will be that value producing exit probability conf
at time t(n) given the bounds.  If t(n) < 1, this is not the 
power of the study."
}

\usage{
computeDrift(n, t, t2, lowerBounds, upperBounds, target, drift, nMax)
}

\arguments{
  \item{n}{the number of analyses.}
  \item{t}{ the vector of analysis time point on (0,1).}
  \item{t2}{ is the second or information time scale, for covariances}
  \item{lowerBounds}{ the vector of lower bounds (standardized). }
  \item{upperBounds}{ the vector of upper bounds (standardized). }
  \item{target}{ desired power respectively confidence level }
  \item{drift}{ vector of drift parameters }
  \item{nMax}{ Number of interim analyses is by default limited to \code{nMax=25} }
}

\details{
The Fortran-Implementation used a naive search algorithm to calculate
the drift. I replaced it by an algorithm which is
called in german "Sekanten-Verfahren".
This one is based on the Newton Iteration but uses secants here
considering we cannot derive our function

The calculation obeys following pattern whereby $x_{k+1}$ converges against
the value we are searching for.\cr
$x_{k+1} = \frac{ x_{k}-x_{k-1}}{f(x_{k})-f(x_{k-1})}*f(x_{k})$

The Fortran-Implementation usually does about 17 iterations to calculate
the drift whereas my method does only 3 or 4 iterations!

If my method fails in converging which usually should not happen
the function tries to calculate one more time with that more
conservative and carefully method of the Fortran Implementation.
}

\value{
  \item{drift }{computed drift}
}

\references{ \url{http://www.biostat.wisc.edu/landemets/} }

\author{ Roman Pahl }

\note{This function is meant to be called in context of using \code{\link{groupseq}} - in this case it is called by \code{\link{findDrift}}}

\seealso{ \code{\link{groupseq}}, \code{\link{computeAlphaLevel}} 
          \code{\link{findDrift}} }

\examples{
## fictious example
computeDrift(2, c(0.5,1.0), c(0.5,1.0), c(-8,-8), c(2.3,1.1), 0.3, 0, 25)
## with following result:
# [1] 0.5696356
}

\keyword{methods} 
