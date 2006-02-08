\name{guiOutputTask4}

\alias{guiOutputTask4}

\title{ output results from Task -4- }

\description{
This will output the results computed in Task -4-
Output is via GUI with possibilty to save the results.
}

\usage{
guiOutputTask4(n, confidenceLevel, secondTimeScaleIsUsed, t, t2, t2max, lowerBounds, upperBounds, BoundsSymmetry, enterBoundsManually, alpha, confidenceIntervall, spendingFunctionUsed, Zvalue)
}

\arguments{
  \item{n}{ is the number of interim analyses. }
  \item{confidenceLevel}{ desired power respectively confidence level }
  \item{secondTimeScaleIsUsed}{ second time/information scale (yes=>secondTimeScaleIsUsed==TRUE, no=>secondTimeScaleIsUsed==FALSE) }
  \item{t}{ is the vector of information times. }
  \item{t2}{ is the second or information time scale, for covariances }
  \item{t2max}{ maximum of t2 }
  \item{lowerBounds}{ is the vector of lower standardized boundaries }
  \item{upperBounds}{ is the vector of upper standardized boundaries }
  \item{BoundsSymmetry}{ BoundsSymmetry==1 means one-sided bounds, BoundsSymmetry==2 means two-sided symmetric and BoundsSymmetry==3 means asymmetric bounds. }
  \item{enterBoundsManually}{ denotes whether bounds are entered by user or have to be computed by spending function }
  \item{alpha}{ is the total type I error. }
  \item{confidenceIntervall}{ is the computed confidence intervall }
  \item{spendingFunctionUsed}{ indicates type I error spending rate function e.g. the function(s) the user choosed. }
  \item{Zvalue}{ standarized statistic (Z value) at the last analysis }
}

\author{ Roman Pahl }

\note{
This function is meant to be called in context of using \code{\link{groupseq}}
}

\seealso{ \code{\link{groupseq}}, \code{\link{guiOutputTask1}} 
          \code{\link{guiOutputTask2}}, \code{\link{guiOutputTask3}} }

\examples{
## function is called within gui - so example won´t work as a stand alone and is quite useless anyway
}

\keyword{methods}
\keyword{IO}