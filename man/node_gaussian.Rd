\name{node_gaussian}
\alias{node_gaussian}

\title{
Simulate a Node Using Linear Regression
}
\description{
Data from the parents is used to generate the node using linear regression by predicting the covariate specific mean and sampling from a normal distribution with that mean and a specified standard deviation.
}
\usage{
node_gaussian(data, parents, betas, intercept, error)
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
  \item{error}{
A single number specifying the sigma error that should be used when generating the node.
  }
}
\details{
Using the general linear regression equation, the observation-specific value that would be expected given the model is generated for every observation in the dataset generarted thus far. We could stop here, but this would create a perfect fit for the node, which is unrealistic. Instead, we add an error term by taking one sample of a normal distribution for each observation with mean zero and standard deviation \code{error}. This error term is then added to the predicted mean.
}
\author{
Robin Denz
}
\examples{
library(simDAG)

set.seed(12455432)

dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("bmi", type="gaussian", parents=c("sex", "age"),
       betas=c(1.1, 0.4), intercept=12, error=2)

sim_dat <- sim_from_dag(dag=dag, n_sim=100)
}
