\name{node_negative_binomial}
\alias{node_negative_binomial}

\title{
Simulate a Node Using Negative Binomial Regression
}
\description{
Data from the parents is used to generate the node using negative binomial regression by applying the betas to the design matrix and sampling from the \code{rnbinom} function.
}
\usage{
node_negative_binomial(data, parents, betas, theta)
}
\arguments{
  \item{data}{
A \code{data.frame} containing all columns specified by \code{parents}.
  }
  \item{parents}{
A character vector specifying the names of the parents that this particular child node has.
  }
  \item{betas}{
A numeric vector with length equal to \code{parents}, specifying the causal beta coefficients used to generate the node.
  }
  \item{theta}{
A single number specifying the theta parameter (\code{size} argument in \code{rnbinom}).
  }
}
\details{
This function uses the linear predictor defined by the \code{betas} and the input design matrix to sample from a subject-specific negative binomial distribution.
}
\author{
Robin Denz
}
\examples{
library(simDAG)

set.seed(124554)

dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("smoking", type="negative_binomial", parents=c("sex", "age"),
       betas=c(1.1, 0.4), theta=0.5)

sim_dat <- sim_from_dag(dag=dag, n_sim=100)
}
