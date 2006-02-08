\name{outputBounds}

\alias{outputBounds}

\title{ console output }

\description{
This function prints out bounds into the console computed by a spending function
}

\usage{
outputBounds(n, alpha, t, lowerBounds, upperBounds, probDifference, probExit, symmetricBoundsYesNo, spendingFunctionUsed)
}

\arguments{
  \item{n}{ is the number of interim analyses. }
  \item{alpha}{ is the total type I error. }
  \item{t}{ is the information times. }
  \item{lowerBounds}{ the vector of lower standardized boundaries }
  \item{upperBounds}{ the vector of upper standardized boundaries }
  \item{probDifference}{ probExit(i)-probExit(i-1)}
  \item{probExit}{ ~vector of exit probabilities}
  \item{symmetricBoundsYesNo}{ symmetricBoundsYesNo==1 means one-sided bounds, symmetricBoundsYesNo!=1 means two-sided bounds. }
  \item{spendingFunctionUsed}{ indicates type I error spending rate function e.g. the function(s) the user choosed }
}

\author{ Roman Pahl }

\note{
This function is meant to be called in context of using \code{\link{groupseq}}
}

\seealso{ \code{\link{groupseq}}, \code{\link{print}} 
          \code{\link{cbind}}, \code{\link{format}},
          \code{\link{data.frame}} }

\examples{
## a very simple fictious one:
outputBounds(1, 0.05, 1, -1, 1, 0.02, 0.05,TRUE , 1)
## with following result:

# ################################# 
# #                               # 
# # Output of the computed Bounds # 
# #                               # 
# ################################# 
# 
# 
# number of interim analyses = 1 
# alpha = 0.05 
# O'Brien-Fleming Type was used as spending Function. 
#  
#       *Times*  *Lower Bounds*  *Upper Bounds*  *alpha[i]-alpha[i-1]*  *cumulative alpha*
# 1        1              -1               1                   0.02                0.05

}

\keyword{methods}
\keyword{IO}