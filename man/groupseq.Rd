\name{groupseq}

\alias{groupseq}

\title{ starts the programm }
\description{
This function is invoked when the package is loaded into R workspace. It starts the programm
using a GUI (default i.e. groupseq(mode=="g")) which is recommended. If, for any reason, the gui does not work, 
user can choose interactive mode by typing groupseq( mode=="c") to work within the R console.

The program itself refers to an earlier Fortran Implementation by Reboussin et al.
The program was completely recoded in R. Following the maxim of modular programming,
it was decomposed in various functions. Furthermore, some algorithms could be improved 
and the application was embedded into a GUI affording much more convenient and faster working.
Last but not least computation results can be saved into tables of *.html file format 
to eventually process them further on.
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
which might cause problems sometimes. Moreover the GUI should work on all platforms R is running on.

Each task and calculation result will open in a new window with the possibility to save results into a *.html File.
Thus, the user may perform multiple tasks e.g. computing and comparing several designs at the same time.
}

\references{ \url{http://www.biostat.wisc.edu/landemets/} }

\author{ Roman Pahl }

\examples{
## call GUI
# groupseq() or groupseq(mode="g")

## call console mode
# groupseq(mode="c")
}

\keyword{methods}