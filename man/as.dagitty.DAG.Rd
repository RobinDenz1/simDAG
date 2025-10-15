\name{as.dagitty.DAG}
\alias{as.dagitty.DAG}

\title{
Transform a \code{DAG} object into a \code{dagitty} object
}
\description{
This function extends the \code{as.dagitty} function from the \code{dagitty} package to allow the input of a \code{DAG} object. The result is a \code{dagitty} object that includes only the structure of the DAG, without any specifications. May be useful to perform identifiability checks etc. on the \code{DAG}.
}
\usage{
\method{as.dagitty}{DAG}(x, include_root_nodes=TRUE,
           include_td_nodes=TRUE, include_networks=FALSE,
           layout=FALSE, ...)
}
\arguments{
  \item{x}{
A \code{DAG} object created using the \code{\link{empty_dag}} function with nodes added to it using the \code{+} syntax. See \code{?empty_dag} or \code{?node} for more details. Supports DAGs with time-dependent nodes added using the \code{\link{node_td}} function. However, including such DAGs may result in cyclic causal structures, because time is not represented in the output matrix.
  }
  \item{include_root_nodes}{
Whether to include root nodes in the output matrix. Should usually be kept at \code{TRUE} (default).
  }
  \item{include_td_nodes}{
Whether to include time-dependent nodes added to the \code{dag} using the \code{\link{node_td}} function or not. When including these types of nodes, it is possible for the adjacency matrix to contain cycles, e.g. that it is not a classic DAG anymore, due to the matrix not representing the passage of time.
  }
  \item{include_networks}{
Whether to include time-fixed networks added to the \code{dag} using the \code{\link{network}} function or not. Usually it does not make sense to include those, because they are not classical nodes.
  }
  \item{layout}{
Corresponds to the argument of the same name in the \code{\link[dagitty]{dagitty}} function.
  }
  \item{...}{
Currently not used.
  }
}
\author{
Robin Denz
}
\value{
Returns a \code{dagitty} object.
}
\seealso{
\code{\link{empty_dag}}, \code{\link{node}}, \code{\link{node_td}}, \code{\link{as.igraph}}
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

if (requireNamespace("dagitty")) {
  g <- dagitty::as.dagitty(dag)
}
}
