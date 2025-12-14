\name{rtexp}
\alias{rtexp}

\title{
Sample values from a left-truncated exponential distribution
}
\description{
This function is a simple wrapper around the \code{\link[stats]{rexp}} function, allowing users to directly sample values from a left-truncated exponential distribution.
}
\usage{
rtexp(n, rate, l=NULL)
}
\arguments{
  \item{n}{
How many draws to make.
  }
  \item{rate}{
A numeric vector of numbers > 0, specifying the rate parameter of the exponential distribution.
  }
  \item{l}{
A numeric vector of numbers > 0, specifying the value at which the distribution should be left-truncated.
  }
}
\author{
Robin Denz
}
\details{
This function mostly exists so it can be used conveniently when performing discrete-event simulations.
}
\value{
Returns a numeric vector of length \code{n}.
}
\examples{
library(simDAG)

rtexp(n=10, rate=0.05, l=20)

# without replacement
dag <- empty_dag() +
  node("A", type="rtexp", rate=0.01, l=100)
data <- sim_from_dag(dag, n_sim=5)
head(data)
}
