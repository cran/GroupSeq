\name{calculateTask4}

\alias{calculateTask4}

\title{computes confidence interval.}

\description{
This function computes confidence interval.
}

\usage{
calculateTask4(n, nMax, t, t2, t2max, t3, confidenceLevel, equallySpacedTimesInput, secondTimeScaleIsUsed, BoundsSymmetry, alpha, phi, usedFunction, TruncateBoundsInput, enterBoundsManually, upperBounds, lowerBounds, Zvalue)
}

\arguments{
  \item{n}{is the number of interim analyses}
  \item{nMax}{maximum number of interim analysis (here: limited to 25)}
  \item{t}{is the vector of analysis times on (0,1] scale}
  \item{t2}{is the second or information time scale, for covariances}
  \item{t2max}{maximum of t2}
  \item{t3}{t2 divided by the maximum information, if needed}
  \item{confidenceLevel}{desired power respectively confidence level}
  \item{equallySpacedTimesInput}{variable denoting whether equally spaced times (equallySpacedTimes==TRUE) or different spaced (equallySpacedTimes==FALSE)}
  \item{secondTimeScaleIsUsed}{ second time/information scale (yes=>secondTimeScaleIsUsed==TRUE, no=>secondTimeScaleIsUsed==FALSE)}
  \item{BoundsSymmetry}{BoundsSymmetry==1 means one-sided bounds, BoundsSymmetry==2 means two-sided symmetric and BoundsSymmetry==3 means asymmetric bounds.}
  \item{alpha}{is the desired overall size}
  \item{phi}{optional Parameter referring to Power Family}
  \item{usedFunction}{indicates type I error spending rate function e.g. the function(s) the user choosed}
  \item{TruncateBoundsInput}{is Truncation Point for bounds (default is no truncation)}
  \item{enterBoundsManually}{denotes whether bounds are entered by user or have to be computed by spending function}
  \item{upperBounds}{vector of upper bounds}
  \item{lowerBounds}{vector of lower bounds}
  \item{Zvalue}{standarized statistic (Z value) at the last analysis}
}

\author{ Roman Pahl }

\note{
This function is meant to be called in context of using \code{\link{groupseq}}
}

\seealso{ \code{\link{groupseq}}, \code{\link{calculateTask1}} 
          \code{\link{calculateTask2}}, \code{\link{calculateTask3}}}

\examples{
## function is called within gui - so example is senseless
}

\keyword{methods}
