\name{calculateEqualBounds}

\alias{calculateEqualBounds}

\title{ Computes the exact Pocock bounds }

\description{
This funtion computes the exact Pocock bounds instead of approximating them by spending function.

The Spending function gives us an approximate Pocock-Design.\cr
To compute the exact Pocock Bounds we will do according to the following pattern:\cr
(1st)we give the bounds with all bounds are equal. As starting value we are using the \cr
     mean of the bounds computed by our approximating Pocock spending function. I figured out that \cr
     in almost every case this is a quite good approximation so far.\cr
(2nd)we compute the probability according to our equal bounds, as we would do, if we would compute \cr
     probability to given bounds.\cr
(3rd)we use Newton Iteration to adjust the bounds in every Iteration until we get the appropriate alpha\cr
}

\usage{
calculateEqualBounds(targetAlpha, provisionallyBounds, n, t2)
}

\arguments{
  \item{targetAlpha}{is the desired overall size.}
  \item{provisionallyBounds}{is first approximation of bounds by spending function Pocock-Type}
  \item{n}{is the number of interim analyses }
  \item{t2}{is the second or information time scale, for covariances}
}

\details{
We use an algorithm which is called in German "Sekanten-Verfahren".\cr
This one is based on the Newton Iteration but uses secants here \cr
considering we cannot derive our function.\cr 
The calculation obeys following pattern whereby $x_{k+1}$ converges against
the value we are searching for.\cr
$x_{k+1} = \frac{ x_{k}-x_{k-1}} {f(x_{k})-f(x_{k-1})}*f(x_{k})$

}

\value{
\item{upperBounds }{exact Pocock bounds to given target alpha}
}

\author{Roman Pahl}

\note{
This function is meant to be called in context of using \code{\link{groupseq}}
}

\seealso{ \code{\link{groupseq}}, \code{\link{computeBounds}}
          \code{\link{computeAlphaLevel}}, \code{\link{asPocock}} }


\examples{
## fictious example with result: [1] 1.993306 1.993306 1.993306
calculateEqualBounds(0.05,c(1.9,2.1,2.0),3,c(0.33,0.66,1.0))
}

\keyword{methods}
\keyword{math}

