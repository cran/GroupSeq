\name{stdDeviations}

\alias{stdDeviations}

\title{Standard deviations are calculated }

\description{
Standard deviations are calculated.
}

\usage{
stdDeviations(n, t2)
}

\arguments{
  \item{n}{ is the number of interim analyses.}
  \item{t2}{ is the 'second' time scale, used for the covariance of
             sequential statistics, usually number of events or subjects. }
}

\value{
  \item{standardDeviation }{is the standard deviation of the process increment.}
  \item{standardDeviationProcess }{is the standard deviation of the process.}
}

\author{ Roman Pahl }

\note{
This function is meant to be called in context of using \code{\link{groupseq}}
}

\seealso{ \code{\link{groupseq}} }
          
\examples{
## simple example
stdDeviations(3, c(0.33,0.66,1.0))
## with following result:
# $standardDeviation
# [1] 0.5744563 0.5744563 0.5830952
#
# $stdvOfTheProcess
# [1] 0.5744563 0.8124038 1.0000000

}

\keyword{methods}
\keyword{distribution}