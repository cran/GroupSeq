\name{outputForDifferentDrifts}

\alias{outputForDifferentDrifts}

\title{ console output }

\description{
This function prints out results from function 'computeAlphaLevel' into the console
}

\usage{
outputForDifferentDrifts(n, probTotal, drift, expectedStoppingTime, secondTimeScaleIsUsed, t, t2, t2max, lowerBounds, upperBounds, probStopping, probExceedingUpper, probExceedingLower)
}

\arguments{
  \item{n}{ is the number of interim analyses. }
  \item{probTotal}{ is the total type I error used. }
  \item{drift}{ vector of drift parameters }
  \item{expectedStoppingTime}{ is expected stopping time (not implemented). }
  \item{secondTimeScaleIsUsed}{ second time/information scale (yes=>secondTimeScaleIsUsed==TRUE, no=>secondTimeScaleIsUsed==FALSE) }
  \item{t}{ is the vector of information times. }
  \item{t2}{ is the second or information time scale, for covariances }
  \item{t2max}{ maximum of t2 }
  \item{lowerBounds}{ is the vector of lower standardized boundaries }
  \item{upperBounds}{ is the vector of upper standardized boundaries }
  \item{probStopping}{ is the prob of reaching ith analysis and stopping.}
  \item{probExceedingUpper}{ is the prob of reaching ith and exceeding upper. }
  \item{probExceedingLower}{ is the prob of reaching ith and exceeding lower. }
}

\value{
  \item{cumulativeExitProb }{ needed for the power in the graph,if Task 3 was chosen}
}

\author{ Roman Pahl }

\note{ Note: probTotal, expectedStoppingTime and probStopping are included but not used.\cr
       Note: t2 is the information time scale provided that two have been specified.\cr \cr
       This function is meant to be called in context of using \code{\link{groupseq}}   }

\seealso{ \code{\link{groupseq}}, \code{\link{print}} 
          \code{\link{cbind}}, \code{\link{format}},
          \code{\link{data.frame}} }

\examples{
## a very simple fictious one:
outputForDifferentDrifts(1, 0.05, 1.64485, 0.5, FALSE, 1, 0, 0, -8, 1.6449, 0.05, 0.05, 0.05)
## with the following result:
#
# n= 1 ,  Drift= 1.64485 
# Maximum Information: 0 
#  -----------------------------------------------------------------------------------------------
#  Times  Lower Bounds  Upper Bounds  exit Prob  cumulative Exit Prob
#  1      1            -8        1.6449        0.1                   0.1
#  -----------------------------------------------------------------------------------------------
#
#[1] 0.1

}

\keyword{methods}
\keyword{IO}