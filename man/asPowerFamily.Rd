\name{asPowerFamily}

\alias{asPowerFamily}

\title{approximates alpha-function Power Family}

\description{
This alpha spending function approximates alpha-function Power Family.
}

\usage{
asPowerFamily(alpha, tk, OneOrTwoSidedBounds, delta)
}

\arguments{
  \item{alpha}{is the desired overall size.}
  \item{tk}{is current point of time}
  \item{OneOrTwoSidedBounds}{One-('OneOrTwoSidedBounds'==1) or Two-sided bounds('OneOrTwoSidedBounds'==2) are used}
  \item{delta}{is the parameter for the Power Family}
}

\value{
see description
}

\author{Roman Pahl}

\note{This function is meant to be called in context of using \code{\link{groupseq}}}

\seealso{ \code{\link{groupseq}}, \code{\link{alphaByUseFunction}}, \code{\link{asPowerFamily}}
          \code{\link{asHSdeCani}}, \code{\link{asPocock}} }

\examples{
##simple example with result: [1] 0.2639016
asPowerFamily(0.05,4,2,1.7)
}

\keyword{methods}