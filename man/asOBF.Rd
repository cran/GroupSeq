\name{asOBF}

\alias{asOBF}

\title{approximates O'Brien and Fleming-type}

\description{
This alpha spending function approximates alphaStar-function O'Brien and Fleming-type.
}

\usage{
asOBF(alpha, tk, OneOrTwoSidedBounds)
}

\arguments{
  \item{alpha}{is the desired overall size.}
  \item{tk}{is current point of time}
  \item{OneOrTwoSidedBounds}{One-('OneOrTwoSidedBounds'==1) or Two-sided bounds('OneOrTwoSidedBounds'==2) are used}
}

\value{
see description
}

\author{Roman Pahl}

\note{This function is meant to be called in context of using \code{\link{groupseq}}}

\seealso{ \code{\link{groupseq}}, \code{\link{alphaByUseFunction}}, \code{\link{asPowerFamily}}
          \code{\link{asHSdeCani}}, \code{\link{asPocock}} }

\examples{
##simple example with result: [1] 0.2578086
asOBF(0.05,3,1)
}

\keyword{methods}