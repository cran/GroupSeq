\name{asHSdeCani}

\alias{asHSdeCani}

\title{approximates the Hwang-Shih-de Cani family}
\description{
This alpha spending function approximates the Hwang-Shih-de Cani family
}

\usage{
asHSdeCani(alpha, tk, OneOrTwoSidedBounds, gamma)
}

\arguments{
  \item{alpha}{is the desired overall size.}
  \item{tk}{is current point of time}
  \item{OneOrTwoSidedBounds}{One-('OneOrTwoSidedBounds'==1) or Two-sided bounds('OneOrTwoSidedBounds'==2) are used}
  \item{gamma}{is the parameter for the Hwang-Shih-de Cani family}
}

\value{
  see description
}

\author{Roman Pahl}

\note{This function is meant to be called in context of using \code{\link{groupseq}}}

\seealso{ \code{\link{groupseq}}, \code{\link{alphaByUseFunction}}, \code{\link{asPowerFamily}}
          \code{\link{asOBF}}, \code{\link{asPocock}} }

\examples{
# simple example with result: [1] 0.025
asHSdeCani(0.05,1,2,0.3)
}

\keyword{methods}
