\name{alphaByUseFunction}

\alias{alphaByUseFunction}

\title{Computes alpha by a use function}

\description{
This function computes type I error to spend 
at each analysis using an alpha-spending function.
}

\usage{
alphaByUseFunction(whatSpendingFunctionIsUsed, n, alpha, phi, OneOrTwoSidedBounds, t)
}

\arguments{
  \item{whatSpendingFunctionIsUsed}{ indicates type I error spending rate function and is an integer value. }
  \item{n}{ is the number of interim analyses so far }
  \item{alpha}{ is the desired overall size }
  \item{phi}{ optional Parameter referring to Power Family }
  \item{OneOrTwoSidedBounds }{One-(OneOrTwoSidedBounds==1) or Two-sided bounds(OneOrTwoSidedBounds==2)}
  \item{t}{ is the vector of information times }
}

\details{
The use function, or type I error spending rate, corresponds to the \cr
Alpha 1 star, Alpha 2 star and Alpha 3 star in Lan & DeMets(1983).
}

\value{
  \item{probExit }{ the vector of type I error (e.g. exit probabilities) spent at each analysis.}
}

\references{ Lan & DeMets 1983 Biometrika \url{http://www.biostat.wisc.edu/landemets/} }

\author{ Roman Pahl }

\note{ 
This function is meant to be called in context of using \code{\link{groupseq}}

       The parameter whatSpendingFunctionIsUsed is an integer value. At the moment there are
       5 functions available - so only parameter from 1-5 will work:\cr
       
       Approximated by spending function:\cr
       (1) O'Brien-Fleming Type\cr
       (2) Pocock Type \cr
       (3) Power family: $\alpha t^{\phi}$\cr
       (4) Hwang-Shih-DeCani fammily \cr
       
       Classic Design:\cr
       (5) Pocock - the real Pocock Bounds
     }
 
\seealso{ \code{\link{groupseq}}, \code{\link{asHSdeCani}}, \code{\link{asPowerFamily}}
          \code{\link{asOBF}}, \code{\link{asPocock}} }

\examples{
## One-sided using O'Brien-Fleming Type
alphaByUseFunction(1,3,0.05,0,1,c(0.33,0.66,1.0))

## Two-sided using Power family: $\alpha t^{\phi}$ with $\phi$==1
alphaByUseFunction(3,3,0.05,1,2,c(0.33,0.66,1.0))
}

\keyword{methods}
\keyword{distribution}
