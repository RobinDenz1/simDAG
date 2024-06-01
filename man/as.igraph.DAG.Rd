\name{as.igraph.DAG}
\alias{as.igraph.DAG}

\title{
Transform a DAG object into an \code{igraph} object
}
\description{
This function extends the \code{as.igraph} function from the \code{igraph} package to allow the input of a \code{DAG} object. The result is an \code{igraph} object that includes only the structure of the DAG, not any specifications. May be useful for plotting purposes.
}
\usage{
\method{as.igraph}{DAG}(x, ...)
}
\arguments{
  \item{x}{
A \code{DAG} object created using the \code{\link{empty_dag}} function with nodes added to it using the \code{+} syntax. See \code{?empty_dag} or \code{?node} for more details. Supports DAGs with time-dependent nodes added using the \code{\link{node_td}} function. However, including such DAGs may result in cyclic causal structures, because time is not represented in the output matrix.
  }
  \item{...}{
Currently not used.
  }
}
\author{
Robin Denz
}
\value{
Returns a \code{igraph} object.
}
\seealso{
\code{\link{empty_dag}}, \code{\link{node}}, \code{\link{node_td}}
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

if (requireNamespace("igraph")) {
  g <- igraph::as.igraph(dag)
  plot(g)
}
}
