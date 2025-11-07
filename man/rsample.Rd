\name{rsample}
\alias{rsample}

\title{
Sample values from a given vector
}
\description{
This function is a simple wrapper around the \code{\link{sample}} function, allowing users to directly sample values from a given input vector (with or without replacement and with or without defining selection probabilities).
}
\usage{
rsample(n, x, replace=FALSE, prob=NULL)
}
\arguments{
  \item{n}{
How many draws to make.
  }
  \item{x}{
A vector containing one or more elements from which to sample from.
  }
  \item{replace}{
Either \code{TRUE} or \code{FALSE}, specifying whether the sampling should be performed with or without replacement.
  }
  \item{prob}{
A numeric vector of probability weights for obtaining the elements of the vector being sampled or \code{NULL} (default). If \code{NULL}, a simple random sample without weights will be performed.
  }
}
\details{
This function is very similar to the \code{\link{rcategorical}} function, with the main difference being that \code{rsample()} directly supports any kind of vector input, not just a few categorical value, but it does not support matrix input in the \code{prob} argument. Use \code{\link{rcategorical}} if the goal is to sample from a categorical distribution with few categories or different probabilities per person and use \code{rsample()} for general sampling purposes.
}
\author{
Robin Denz
}
\value{
Returns a vector of length \code{n} with the same type as \code{x}.
}
\examples{
library(simDAG)

# without replacement
dag <- empty_dag() +
  node("A", type="rsample", x=1:10, replace=FALSE)
data <- sim_from_dag(dag, n_sim=5)
head(data)

# with replacement and selection probabilities
dag <- empty_dag() +
  node("A", type="rsample", x=c(1, 2, 3, 4), replace=TRUE,
       prob=c(0.1, 0.3, 0.1, 0.5))
data <- sim_from_dag(dag, n_sim=100)
head(data)
}
