\name{rbernoulli}
\alias{rbernoulli}

\title{
Generate Random Draws from a Bernoulli Distribution
}
\description{
A very fast implementation for generating bernoulli trials. Can take a vector of probabilities which makes it very useful for simulation studies.
}
\usage{
rbernoulli(n, p=0.5)
}
\arguments{
  \item{n}{
How many draws to make.
  }
  \item{p}{
A numeric vector of probabilities, used when drawing the trials.
  }
}
\details{
Internally, it uses only a single call to \code{runif}, making it much faster and more memory efficient than using \code{rbinomial}.
}
\author{
Robin Denz
}
\examples{
# generating 5 bernoulli random draws from an unbiased coin
rbernoulli(n=5, p=0.5)

# using different probabilities for each coin throw
rbernoulli(n=5, p=c(0.1, 0.2, 0.3, 0.2, 0.7))
}
