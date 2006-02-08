\name{useSpendingFunction}

\alias{useSpendingFunction}

\title{ computes bounds }

\description{
This function runs in the console mode - 
it computes bounds using a user chosen spending function
}

\usage{
useSpendingFunction(t, n, t2)
}

\arguments{
  \item{t}{ is the vector of information times. }
  \item{n}{ is the number of interim analyses. }
  \item{t2}{ is the 'second' time scale, used for the covariance of
             sequential statistics, usually number of events or subjects. }
}

\details{
This function works in an interactive mode in the R console. It asks user for some
input before calculating and printing the output.
}

\value{
  \item{alpha }{is the total type I error.}
  \item{lowerBounds }{the vector of lower standardized boundaries}
  \item{upperBounds }{the vector of upper standardized boundaries}
  \item{probExit }{is a vector of exit probabilities}
  \item{probDifference }{probDifference(i) = probExit(i)-probExit(i-1) where probExit is a vector of exit probabilities.}
  \item{symmetricBounds }{denotes whether symmetric bounds are used(symmetricBounds==TRUE) or not(symmetricBounds==FALSE)}
  \item{whatSpendingFunctionIsUsed }{denotes the spending function which is used, (e.g. Pocock Type=> whatSpendingFunctionIsUsed<-2, etc...)}
  \item{OneOrTwoSidedBounds }{One-(OneOrTwoSidedBounds==1) or Two-sided bounds(OneOrTwoSidedBounds==2)}

}

\author{ Roman Pahl }

\note{
This function is meant to be called in context of using \code{\link{groupseq}}
}

\seealso{ \code{\link{groupseq}}, \code{\link{computeBounds}} 
          \code{\link{calculateEqualBounds}}, \code{\link{computeAlphaLevel}} }

\examples{
## simple example - always interactive dialogue will open to get necessary input from user  - so try by yourself
# useSpendingFunction( c(0.5,1.0), 2, c(0.2,0.4) )
}

\keyword{ methods }
