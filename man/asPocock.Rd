\name{asPocock}

\alias{asPocock}

\title{approximates Pocock-type}

\description{
This alpha spending function approximates alpha-function Pocock-type
}

\usage{
asPocock(alpha, tk, OneOrTwoSidedBounds)
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
          \code{\link{asHSdeCani}}, \code{\link{asOBF}} }

\examples{
##simple example with result: [1] 0.09086198
asPocock(0.05,3,1)
}

\keyword{methods}