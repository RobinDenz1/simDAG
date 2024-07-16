\name{rbernoulli}
\alias{rbernoulli}

\title{
Generate Random Draws from a Bernoulli Distribution
}
\description{
A very fast implementation for generating bernoulli trials. Can take a vector of probabilities which makes it very useful for simulation studies.
}
\usage{
rbernoulli(n, p=0.5, output="logical")
}
\arguments{
  \item{n}{
How many draws to make.
  }
  \item{p}{
A numeric vector of probabilities, used when drawing the trials.
  }
  \item{output}{
A single character string, specifying which format the output should be returned as. Must be one of \code{"logical"} (default), \code{"numeric"}, \code{"character"} or \code{"factor"}.
  }
}
\details{
Internally, it uses only a single call to \code{runif}, making it much faster and more memory efficient than using \code{rbinomial}.

Note that this function accepts values of \code{p} that are smaller then 0 and greater than 1. For \code{p < 0} it will always return \code{FALSE}, for \code{p > 1} it will always return \code{TRUE}.
}
\author{
Robin Denz
}
\value{
Returns a vector of length \code{n} in the desired output format.
}
\examples{
library(simDAG)

# generating 5 bernoulli random draws from an unbiased coin
rbernoulli(n=5, p=0.5)

# using different probabilities for each coin throw
rbernoulli(n=5, p=c(0.1, 0.2, 0.3, 0.2, 0.7))

# return as numeric instead
rbernoulli(n=5, p=0.5, output="numeric")
}
