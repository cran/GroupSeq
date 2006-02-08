\name{groupseq}

\alias{groupseq}

\title{ starts the programm }
\description{
This function is used to start the programm where user can choose between a GUI (default e.g. mode=="g") which is recommended
or if for any reason GUI does not work user can choose interactive mode ( mode=="c") taking place in the cosole of R.

The program itself refers to an earlier Fortran Implementation done by Lan DeMets. I took the skeleton of the program,
improved code by decomposing it into more functions, improved algorithms which were ok for the fast Fortran but took
to long in R. I added a GUI for much more convenient and faster working and last but not least results can be saved 
into simple *.html Files to process with them further on.
}

\usage{
groupseq(mode = chosenMode)
}

\arguments{
  \item{mode}{ default is mode=="g" to use the GUI \cr
               but user may choose mode=="c" to use the R console for example if GUI does not work for any reason.}
}

\details{
The GUI is build within the Tcl/Tk interface of R. I attended to only use the standard Tcl/Tk package,
to keep as much compatibility as possible. So you won´t have to install additional Tcl/Tk packages
which might cause problems sometimes. Moreover the GUI should work on all platforms.

Each task and calculation will open in a new window with possibilty to save results into a *.html File.
So you are able to multitask and to compare results directly and decide which of them you want to save.
}

\references{ \url{http://www.biostat.wisc.edu/landemets/} }

\author{ Roman Pahl }

\seealso{ 
\code{\link{guiMode}}, \code{\link{consoleMode}} 
}

\examples{
## call GUI
# groupseq() or groupseq(mode="g")

## call console mode
# groupseq(mode="c")
}

\keyword{methods}
