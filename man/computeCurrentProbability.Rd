\name{computeCurrentProbability}

\alias{computeCurrentProbability}

\title{Computes probabilities for current analysis.}

\description{
Compute probabilities for current analysis.  Applying
Fubini's Theorem to Armitage, McPherson and Rowe's
formula, the inner integral is the normal distribution function
and the outer is computed by a trapezoidal rule numerical integration.
}

\usage{
computeCurrentProbability(lastGrid, numberOfIntegrationIntervalls, lowerIntegrationLimit, upperIntegrationLimit, i, gridSize, standardDeviation)
}

\arguments{
  \item{lastGrid}{is the vector of density values from the previous step.}
  \item{numberOfIntegrationIntervalls}{ is the number of integration intervalls }
  \item{lowerIntegrationLimit}{ is the vector of lower integration limits. }
  \item{upperIntegrationLimit}{ is the vector of upper integration limits.}
  \item{i}{is the index of the current step.}
  \item{gridSize}{ is the grid size (not used here). }
  \item{standardDeviation}{ is the standard deviation of the process increment. }
}

\value{
  \item{probStopping }{is the probaility of reaching ith analysis and stopping 
                       here the value of the integral from lowerIntegrationLimit to upperIntegrationLimit.}
  \item{probExceedingUpper }{the probaility of reaching ith and exceeding upper.
                             here the value of the integral from upperIntegrationLimit to infinity.}
  \item{probExceedingLower }{is the probability of reaching ith and exceeding lower
                             here the value of the integral from lowerIntegrationLimit to minus infinity}
}

\references{ Armitage, McPherson & Rowe 1969 JRSSA }

\author{ Roman Pahl }

\note{This function is meant to be called in context of using \code{\link{groupseq}}}

\seealso{ \code{\link{groupseq}}, \code{\link{pnorm}} 
          \code{\link{integrateByTrapez}}, }
          
\examples{
## function depends heavily on calculations before (e.g.the 'grid'), 
## so you don´t want to use it by manual and therefore an example is not useful
}

\keyword{methods}
