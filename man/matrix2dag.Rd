\name{matrix2dag}
\alias{matrix2dag}

\title{
Obtain a \code{DAG} object from a Adjacency Matrix and a List of Node Types
}
\description{
The \code{\link{sim_from_dag}} function requires the user to specify the causal relationships inside a \code{DAG} object containing node information. This function creates such an object using a adjacency matrix and a list of node types. The resulting \code{DAG} will be only partially specified, which may be useful for the \code{\link{fill_dag_using_data}} function.
}
\usage{
matrix2dag(mat, type)
}
\arguments{
  \item{mat}{
A p x p adjacency matrix where p is the number of variables. The matrix should be filled with zeros. Only places where the variable specified by the row has a direct causal effect on the variable specified by the column should be 1. Both the columns and the rows should be named with the corresponding variable names.
  }
  \item{type}{
A named list with one entry for each variable in \code{mat}, specifying the \code{type} of the corresponding node. See \code{\link{node}} for available node types.
  }
}
\details{
An adjacency matrix is simply a square matrix in which each node has one column and one row associated with it. For example, if the node A has a causal effect on node B, the matrix will contain \code{1} in the spot \code{matrix["A", "B"]}. This function uses this kind of matrix and additional information about the node type to create a \code{DAG} object. The resulting \code{DAG} cannot be used in the \code{\link{sim_from_dag}} function directly, because it will not contain the necessary parameters such as beta-coefficients or intercepts etc. It may, however, be passed directly to the \code{\link{fill_dag_using_data}} function. This is pretty much it's only valid use-case. If the goal is to to specify a full \code{DAG} manually, the user should use the \code{\link{empty_dag}} function in conjunction with \code{\link{node}} calls instead, as described in the respective documentation pages and the vignettes.
}
\author{
Robin Denz
}
\examples{
library(simDAG)

# simple example adjacency matrix
mat <- matrix(c(0, 0, 1, 0, 0, 1, 0, 0, 0), ncol=3, byrow=TRUE)
colnames(mat) <- c("age", "sex", "death")
rownames(mat) <- c("age", "sex", "death")

type <- list(age="rnorm", sex="rbernoulli", death="binomial")

dag <- matrix2dag(mat=mat, type=type)
}
