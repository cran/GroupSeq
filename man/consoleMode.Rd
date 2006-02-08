\name{consoleMode}

\alias{consoleMode}

\title{ provides the user interface in the R console }

\description{
This function provides the user interface in an interactive mode.
It gets the necessary parameters by the user from the R console in case
the gui does not work properly or user is a console fan ;-)
 
User inputs option, analysis times, spending function or bounds,
truncation value for bounds, optional drift parameters, power, 
or confidence level.
}

\usage{
consoleMode()
}

\author{ Roman Pahl }

\note{This function may also be called in context of using \code{\link{groupseq}}
      groupseq(mode="c") does the same and is recommended}

\seealso{ \code{\link{groupseq}}, \code{\link{useSpendingFunction}} 
          \code{\link{outputBounds}}, \code{\link{outputDriftWithBounds}}
          \code{\link{outputForDifferentDrifts}}, \code{\link{findDrift}}
          \code{\link{computeAlphaLevel}}, \code{\link{computeConfidenceIntervall}} 
}
          
\keyword{methods}
