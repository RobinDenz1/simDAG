\name{node_poisson}
\alias{node_poisson}

\title{
Simulate a Node Using Poisson Regression
}
\description{
Data from the parents is used to generate the node using poisson regression by predicting the covariate specific lambda and sampling from a poisson distribution accordingly.
}
\usage{
node_poisson(data, parents, betas, intercept)
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
  \item{intercept}{
A single number specifying the intercept that should be used when generating the node.
  }
}
\details{
More explanation
}
\author{
Robin Denz
}
\examples{
library(simDAG)

set.seed(345345)

dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("smoking", type="poisson", parents=c("sex", "age"),
       betas=c(1.1, 0.4), intercept=-2)

sim_dat <- sim_from_dag(dag=dag, n_sim=100)
}
