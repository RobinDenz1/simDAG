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
\dontrun{
# 2 root nodes, 1 child node
root_nodes <- list(list(dist="rnorm",
                        params=list(mean=50, sd=4),
                        name="age"),
                   list(dist="rbernoulli",
                        params=list(p=0.5),
                        name="sex"))
child_nodes <- list(list(parents=c("sex", "age"),
                         type="negative_binomial",
                         name="smoking",
                         betas=c(1.1, 0.4),
                         theta=0.5))
sim_dat <- sim_from_dag(n_sim=10000, root_nodes=root_nodes,
                        child_nodes=child_nodes)
}
}
