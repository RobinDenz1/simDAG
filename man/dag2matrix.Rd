\name{dag2matrix}
\alias{dag2matrix}

\title{
Obtain a Adjacency Matrix from a \code{DAG} object
}
\description{
The \code{\link{sim_from_dag}} function requires the user to specify the causal relationships inside a \code{DAG} object containing node information. This function takes this object as input and outputs the underlying adjacency matrix. This can be useful to plot the theoretical DAG or to check if the nodes have been specified correctly.
}
\usage{
dag2matrix(dag, include_root_nodes=TRUE)
}
\arguments{
  \item{dag}{
A \code{DAG} object created using the \code{\link{empty_dag}} function with nodes added to it using the \code{+} syntax. See \code{?empty_dag} or \code{?node} for more details.
  }
  \item{include_root_nodes}{
Whether to include root nodes in the output matrix. Should usually be kept at \code{TRUE} (default).
  }
}
\details{
An adjacency matrix is simply a square matrix in which each node has one column and one row associated with it. For example, if the node A has a causal effect on node B, the matrix will contain \code{TRUE} in the spot \code{matrix["A", "B"]}.
}
\author{
Robin Denz
}
\examples{
library(simDAG)

# some example DAG
dag <- empty_dag() +
  node("death", type="binomial", parents=c("age", "sex"), betas=c(1, 2),
       intercept=-10) +
  node("age", type="rnorm", mean=10, sd=2) +
  node("sex", parents="", type="rbernoulli", p=0.5) +
  node("smoking", parents=c("sex", "age"), type="binomial",
       betas=c(0.6, 0.2), intercept=-2)

# get adjacency matrix
dag2matrix(dag)

# get adjacency matrix using only the child nodes
dag2matrix(dag, include_root_nodes=FALSE)
}
