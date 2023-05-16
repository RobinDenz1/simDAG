\name{node_custom}
\alias{node_custom}

\title{
Create Your Own Function to Simulate a Node
}
\description{
This page describes in detail how to extend this code to allow the usage of nodes that are not directly implemented here. A custom node function may be created by the user. It has to have the following arguments:
}
\arguments{
  \item{data}{
A \code{data.frame} containing all columns specified by \code{parents}.
  }
  \item{parents}{
A character vector specifying the names of the parents that this particular child node has.
  }
  \item{...}{
Any amount of additional arguments.
  }
}
\details{
The number of available types of nodes is limited, but this package allows the user to easily implement their own node types by writing a single custom function. Only three things are required for this to work properly: (1) the function name should start with \code{node_}, (2) the function should contain the arguments \code{data} and \code{parents} as described above and (3) it should return either a vector of length \code{n_sim} or a \code{data.frame} with any number of columns and \code{n_sim} rows.

The function may include any amount of additional arguments specified by the user. If you think that your custom node type might be useful to others, please contact the maintainer of this package via the supplied e-mail address or github and we might add it to this package.

Some simple examples for custom nodes are given below.
}
\author{
Robin Denz
}
\examples{
library(simDAG)

set.seed(3545)

# create a custom node function, which is just a gaussian node that
# includes (bad) truncation
node_gaussian_trunc <- function(data, parents, betas, intercept, error,
                                left, right) {
  out <- node_gaussian(data=data, parents=parents, betas=betas,
                       intercept=intercept, error=error)
  out <- ifelse(out <= left, left,
                ifelse(out >= right, right, out))
  return(out)
}

# another custom node function, which simply returns a sum of the parents
node_parents_sum <- function(data, parents, betas=NULL) {
  out <- rowSums(data[, parents, with=FALSE])
  return(out)
}

# an example of using these new node types in a simulation
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("custom_1", type="gaussian_trunc", parents=c("sex", "age"),
       betas=c(1.1, 0.4), intercept=-2, error=2, left=10, right=25) +
  node("custom_2", type="parents_sum", parents=c("age", "custom_1"))

sim_dat <- sim_from_dag(dag=dag, n_sim=100)
}
