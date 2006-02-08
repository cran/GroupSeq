\name{guiMode}

\alias{guiMode}

\title{ creates a GUI }

\description{
This Function creates a GUI where user can choose one of four Tasks to
perform computations related to group sequential boundaries using the spending functions.\cr

There are the following four options which compute:\cr
-1- bounds given analysis times and a spending function;\cr
-2- drift parameters corresponding to given power and bounds;\cr
-3- probabilities given times, bounds and drift parameters;\cr
-4- a confidence interval given times, bounds and final test statistic value.\cr
}

\usage{
guiMode()
}

\details{
The GUI is build within the Tcl/Tk interface of R. I attended to only use the standard Tcl/Tk package,
to keep as much compatibility as possible. So you won´t have to install additional Tcl/Tk packages
which might cause problems sometimes. Moreover the GUI should work on all platforms.

Each task and calculation will open in a new window with possibilty to save results into a *.html File.
So you are able to multitask and to compare results directly and decide which of them you want to save.
}

\author{ Roman Pahl }

\note{This function is meant to be called in context of using \code{\link{groupseq}}
      groupseq() does the same and is recommended}

\seealso{ \code{\link{groupseq}}, \code{\link{guiInputTask1}} 
          \code{\link{guiInputTask2}}, \code{\link{guiInputTask3}},
          \code{\link{guiInputTask4}} }

\keyword{methods}
