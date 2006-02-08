\name{guiOutputTask1}

\alias{guiOutputTask1}

\title{ output results from Task -1- }

\description{
This will output the results computed in Task -1-
Output is via GUI with possibilty to save the results.
}

\usage{
guiOutputTask1(n, alpha, t, lowerBounds, upperBounds, probDifference, probExit, BoundsSymmetry, spendingFunctionUsed)
}

\arguments{
  \item{n}{ is the number of interim analyses. }
  \item{alpha}{ is the total type I error. }
  \item{t}{ is the vector of information times. }
  \item{lowerBounds}{ is the vector of lower standardized boundaries }
  \item{upperBounds}{ is the vector of upper standardized boundaries }
  \item{probDifference}{ probDifference(i) = probExit(i)-probExit(i-1) where probExit is a vector of exit probabilities }
  \item{probExit}{ is a vector of exit probabilities. }
  \item{BoundsSymmetry}{ BoundsSymmetry==1 means one-sided bounds, BoundsSymmetry==2 means two-sided symmetric and BoundsSymmetry==3 means asymmetric bounds. }
  \item{spendingFunctionUsed}{ indicates type I error spending rate function e.g. the function(s) the user choosed. }
}

\author{ Roman Pahl }

\note{
This function is meant to be called in context of using \code{\link{groupseq}}
}

\seealso{ \code{\link{groupseq}}, \code{\link{guiOutputTask2}} 
          \code{\link{guiOutputTask3}}, \code{\link{guiOutputTask4}} }

\examples{
## function is called within gui - so example won´t work as a stand alone and is quite useless anyway
}

\keyword{methods}
\keyword{IO}