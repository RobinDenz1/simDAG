\name{add_node}
\alias{add_node}
\alias{+.DAG}

\title{
Add a \code{DAG.node} object to a \code{DAG} object
}
\description{
This function allows users to add \code{DAG.node} objects created using the \code{\link{node}} or \code{\link{node_td}} function to \code{DAG} objects created using the \code{\link{empty_dag}} function, which makes it easy to fully specify a DAG to use in the \code{\link{sim_from_dag}} function and \code{\link{sim_discrete_time}}.
}
\usage{
add_node(dag, node)

\method{+}{DAG}(object_1, object_2)
}
\arguments{
  \item{dag}{
A \code{DAG} object created using the \code{\link{empty_dag}} function.
  }
  \item{node}{
A \code{DAG.node} object created using the \code{\link{node}} function or \code{\link{node_td}} function.
  }
  \item{object_1}{
Either a \code{DAG} object or a \code{DAG.node} object. The order of the objects does not change the result.
  }
  \item{object_2}{
See argument \code{object_1}.
  }
}
\details{
The two ways of adding a node to a \code{DAG} object are: \code{dag <- add_node(dag, node(...))} and \code{dag <- dag + node(...)}, which give identical results (note that the \code{...} should be replaced with actual arguments and that the initial \code{dag} should be created with a call to \code{empty_dag}). See \code{\link{node}} for more information on how to specify a \code{DAG} for use in the \code{\link{sim_from_dag}} and \code{\link{node_td}} functions.
}
\value{
Returns an \code{DAG} object with the \code{DAG.node} object added to it.
}
\author{
Robin Denz
}
\examples{
library(simDAG)

## add nodes to DAG using +
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=5) +
  node("sex", type="rbernoulli", p=0.5) +
  node("income", type="gaussian", parents=c("age", "sex"), betas=c(1.1, 0.2),
       intercept=-5, error=4)

## add nodes to DAG using add_node()
dag <- empty_dag()
dag <- add_node(dag, node("age", type="rnorm", mean=50, sd=5))
}
