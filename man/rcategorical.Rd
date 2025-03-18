\name{rcategorical}
\alias{rcategorical}

\title{
Generate Random Draws from a Discrete Set of Labels with Associated Probabilities
}
\description{
Allows different class probabilities for each person by supplying a matrix with one column for each class and one row for each person.
}
\usage{
rcategorical(n, probs, labels=NULL, output="numeric",
             reference=NULL)
}
\arguments{
  \item{n}{
How many draws to make. Passed to the \code{size} argument of the \code{sample} function if \code{probs} is not a matrix.
  }
  \item{probs}{
Either a numeric vector of probabilities which sums to one or a matrix with one column for each desired class and \code{n} rows. Passed to the \code{probs} argument of the \code{sample} function if a numeric vector is passed.
  }
  \item{labels}{
A vector of labels to draw from. If \code{NULL} (default), it simply uses integers starting from 1. Passed to the \code{x} argument of the \code{sample} function if \code{probs} is not a matrix.
  }
  \item{output}{
A single character string specifying the output format of the results. Must be either \code{"numeric"} (default), \code{"character"} or \code{"factor"}. If labels are supplied, the output will be parsed as characters by default.
  }
  \item{reference}{
A single character string, specifying which of the possible values should be considered the reference when \code{output="factor"} (ignored otherwise).
  }
}
\details{
In case of a simple numeric vector (class probabilities should be the same for all draws), this function is only a wrapper for the \code{sample} function, to make the code more consistent. It uses weighted sampling with replacement. Otherwise, custom code is used which is faster than the standard \code{rmultinom} function.
}
\author{
Robin Denz
}
\value{
Returns a numeric vector (or factor vector if \code{coerce2factor=TRUE}) of length \code{n}.
}
\examples{
library(simDAG)

rcategorical(n=5, labels=c("A", "B", "C"), probs=c(0.1, 0.2, 0.7))

rcategorical(n=2, probs=matrix(c(0.1, 0.2, 0.5, 0.7, 0.4, 0.1), nrow=2))
}
