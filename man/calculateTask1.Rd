\name{calculateTask1}

\alias{calculateTask1}

\title{computes bounds using input given by the gui}

\description{
This function computes bounds using input given by the gui
}

\usage{
calculateTask1(n, t, t2, equallySpacedTimesInput, secondTimeScaleIsUsedInput, BoundsSymmetry, alpha, phi, usedFunction, TruncateBoundsInput)
}

\arguments{
  \item{n}{is the number of interim analyses }
  \item{t}{is the vector of analysis times on (0,1] scale}
  \item{t2}{is the second or information time scale, for covariances}
  \item{equallySpacedTimesInput}{variable denoting whether equally spaced times (equallySpacedTimes==TRUE) or different spaced (equallySpacedTimes==FALSE)}
  \item{secondTimeScaleIsUsedInput}{second time/information scale (yes=>secondTimeScaleIsUsed==TRUE, no=>secondTimeScaleIsUsed==FALSE)}
  \item{BoundsSymmetry}{BoundsSymmetry==1 means one-sided bounds, BoundsSymmetry==2 means two-sided symmetric and BoundsSymmetry==3 means asymmetric bounds}
  \item{alpha}{is the desired overall size.}
  \item{phi}{optional Parameter referring to Power Family}
  \item{usedFunction}{indicates type I error spending rate function e.g. the function(s) the user choosed}
  \item{TruncateBoundsInput}{is Truncation Point for bounds (default is no truncation)}
}

\author{Roman Pahl}

\note{
This function is meant to be called in context of using \code{\link{groupseq}}
}

\seealso{ \code{\link{groupseq}}, \code{\link{calculateTask2}} 
          \code{\link{calculateTask3}}, \code{\link{calculateTask4}}}

\examples{
## function is called within gui - so example is senseless
}

\keyword{methods}
