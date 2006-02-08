\name{searchForBound}

\alias{searchForBound}

\title{ calculate boundaries }

\description{
here its original description in the Fortran-Implementation 
from \url{http://www.biostat.wisc.edu/landemets/} :

"A naive searching algorithm.  This starts at the previous
boundary, moves in the direction of the current boundary,
changes direction and reduces step size when target is
overstepped.  Stops when probability of estimate is within
tolerance of target probability.
   
Users may want to substitute a better routine: some unused
variables are included to make this easier."
I did substitute a better one - see Details.
}

\usage{
searchForBound(lastGrid, numberOfIntegrationIntervalls, i, gridSize, probDifference, standardDeviation, lowerIntegrationLimit, upperIntegrationLimit, numberOfInterimAnalysis)
}

\arguments{
  \item{lastGrid}{ is joint density from the previous analysis. }
  \item{numberOfIntegrationIntervalls}{ is the number of intervals for numerical integration. }
  \item{i}{ is the number of the current analysis. }
  \item{gridSize}{ is the grid size (not used by current code). }
  \item{probDifference}{ is the target probability. }
  \item{standardDeviation}{is the standard deviation of the process increment. }
  \item{lowerIntegrationLimit}{ is the vector of lower integration limits. }
  \item{upperIntegrationLimit}{ is the vector of upper integration limits. }
  \item{numberOfInterimAnalysis}{ is the number of interim analyses. }
}

\details{
The Fortran Implementation used a naive search algorithm to
calculate boundaries which did not noticeably matter to performance
because of the great speed of Fortran.
But in R it did matter, so i replaced it by an algorithm which is
called in German "Sekanten-Verfahren".
This one is based on the Newton Iteration but uses secants here
considering we cannot derive our function

The calculation obeys following pattern whereby $x_{k+1}$ converges against
the value we are searching for.\cr
$x_{k+1} = \frac{ x_{k}-x_{k-1}}{f(x_{k})-f(x_{k-1})}*f(x_{k})$

If my method fails in converging which usually should not happen
the function tries to calculate one more time with that more
conservative and carefully method of the Fortran Implementation.
}

\value{
  \item{lowerIntegrationLimit }{is the vector of lower integration limits.}
  \item{upperIntegrationLimit }{is the vector of upper integration limits.}
}

\references{ \url{http://www.biostat.wisc.edu/landemets/} }

\author{ Roman Pahl }

\note{This function is meant to be called in context of using \code{\link{groupseq}} }

\seealso{ \code{\link{groupseq}}, \code{\link{tailProbability}} }

\examples{
## function depends heavily on calculations before (e.g. 'lastGrid'), 
## so you don´t want to use it by manual and therefore an example is not useful
}

\keyword{methods}

