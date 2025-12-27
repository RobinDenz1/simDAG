\name{rsample}
\alias{rsample}

\title{
Sample values from a given vector
}
\description{
This function is a simple wrapper around the \code{\link{sample}} function, allowing users to directly sample values from a given input vector (with or without replacement and with or without defining selection probabilities) or \code{data.frame} like object.
}
\usage{
rsample(n, x, replace=FALSE, prob=NULL)
}
\arguments{
  \item{n}{
How many draws to make.
  }
  \item{x}{
A vector containing one or more elements from which to sample from, or a \code{data.frame} like object. If a \code{data.frame} is supplied, random rows from it will be sampled. Note that if the supplied \code{data.frame} has more than one column and this function is used as a node \code{type}, the names of the variables in the supplied \code{x} will be used as variable names and the given node name will be discarded.
  }
  \item{replace}{
Either \code{TRUE} or \code{FALSE}, specifying whether the sampling should be performed with or without replacement.
  }
  \item{prob}{
A numeric vector of probability weights for obtaining the elements of the vector being sampled or \code{NULL} (default). If \code{NULL}, a simple random sample without weights will be performed.
  }
}
\details{
This function is very similar to the \code{\link{rcategorical}} function, with the main difference being that \code{rsample()} directly supports any kind of vector input, not just a few categorical values, but it does not support matrix input in the \code{prob} argument. Use \code{\link{rcategorical}} if the goal is to sample from a categorical distribution with few categories or different probabilities per person and use \code{rsample()} for general sampling purposes.

Note that this function is just a wrapper around the \code{\link{sample}} function, with the only additional functionality being that it also may be used to directly sample from \code{data.frame}s. It is only meant to conveniently allow sampling within the packages syntax (the original function does not use the \code{n} argument, and can thus not be used directly without a wrapper).
}
\author{
Robin Denz
}
\value{
Returns a vector of length \code{n} with the same type as \code{x} if \code{x} is a vector and a \code{data.frame} with \code{n} rows if \code{x} is a \code{data.frame}.
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
  node("X", type="rbernoulli", p=0.5) +
  node("A", type="rsample", x=c(1, 2, 3, 4), replace=TRUE,
       prob=c(0.1, 0.3, 0.1, 0.5))
data <- sim_from_dag(dag, n_sim=100)
head(data)

# sampling rows from a data.frame object
# NOTE: The node name for the rsample() node will be ignored, because
#       a data.frame is supplied to "x". The names of the variables in the
#       data are used directly instead.
dag <- empty_dag() +
  node("placeholder", type="rsample", x=data) +
  node("Y", type="binomial", formula= ~ -2 + A*0.5 + X*-1)
data2 <- sim_from_dag(dag, n_sim=50)
head(data2)
}
