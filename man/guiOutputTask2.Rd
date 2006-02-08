\name{guiOutputTask2}

\alias{guiOutputTask2}

\title{ output results from Task -2- }

\description{
This will output the results computed in Task -2-
Output is via GUI with possibilty to save the results.
}

\usage{
guiOutputTask2(n, probTotal, drift, expectedStoppingTime, secondTimeScaleIsUsed, t, t2, t2max, lowerBounds, upperBounds, probStopping, probExceedingUpper, probExceedingLower, confidenceLevel, BoundsSymmetry, enterBoundsManually, alpha, spendingFunctionUsed)
}

\arguments{
  \item{n}{ is the number of interim analyses.~ }
  \item{probTotal}{is the total type I error used. }
  \item{drift}{ vector of drift parameters }
  \item{expectedStoppingTime}{ is expected stopping time (not implemented).} 
  \item{secondTimeScaleIsUsed}{ second time/information scale (yes=>secondTimeScaleIsUsed==TRUE, no=>secondTimeScaleIsUsed==FALSE) }
  \item{t}{ is the vector of information times. }
  \item{t2}{ is the second or information time scale, for covariances }
  \item{t2max}{ maximum of t2 }
  \item{lowerBounds}{ is the vector of lower standardized boundaries }
  \item{upperBounds}{ is the vector of upper standardized boundaries }
  \item{probStopping}{ is the probability of reaching ith analysis and stopping. }
  \item{probExceedingUpper}{ is the probability of reaching ith and exceeding upper.} 
  \item{probExceedingLower}{ is the probability of reaching ith and exceeding lower. }
  \item{confidenceLevel}{ desired power respectively confidence level }
  \item{BoundsSymmetry}{ BoundsSymmetry==1 means one-sided bounds, BoundsSymmetry==2 means two-sided symmetric and BoundsSymmetry==3 means asymmetric bounds. }
  \item{enterBoundsManually}{ denotes whether bounds are entered by user or were computed by a spending function }
  \item{alpha}{ is the total type I error. }
  \item{spendingFunctionUsed}{ indicates type I error spending rate function e.g. the function(s) the user choosed. }
}

\author{ Roman Pahl }

\note{
This function is meant to be called in context of using \code{\link{groupseq}}
}

\seealso{ \code{\link{groupseq}}, \code{\link{guiOutputTask1}} 
          \code{\link{guiOutputTask3}}, \code{\link{guiOutputTask4}} }

\examples{
## function is called within gui - so example won´t work as a stand alone and is quite useless anyway
}

\keyword{methods}
\keyword{IO}