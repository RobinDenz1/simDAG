\name{dag2matrix}
\alias{dag2matrix}

\title{
Obtain a Adjacency Matrix from a \code{DAG} object
}
\description{
The \code{\link{sim_from_dag}} function requires the user to specify the causal relationships inside a \code{DAG} object containing node information. This function takes this object as input and outputs the underlying adjacency matrix. This can be useful to plot the theoretical DAG or to check if the nodes have been specified correctly.
}
\usage{
dag2matrix(dag, include_root_nodes=TRUE, include_td_nodes=FALSE,
           include_networks=FALSE)
}
\arguments{
  \item{dag}{
A \code{DAG} object created using the \code{\link{empty_dag}} function with nodes added to it using the \code{+} syntax. See \code{?empty_dag} or \code{?node} for more details. Supports DAGs with time-dependent nodes added using the \code{\link{node_td}} function. However, including such DAGs may result in cyclic causal structures, because time is not represented in the output matrix.
  }
  \item{include_root_nodes}{
Whether to include root nodes in the output matrix. Should usually be kept at \code{TRUE} (default).
  }
  \item{include_td_nodes}{
Whether to include time-dependent nodes added to the \code{dag} using the \code{\link{node_td}} function or not. When including these types of nodes, it is possible for the adjacency matrix to contain cycles, e.g. that it is not a classic DAG anymore, due to the matrix not representing the passage of time.
  }
  \item{include_networks}{
Whether to include time-fixed networks added to the \code{dag} using the \code{\link{network}} function or not. Usually it does not make sense to include those, because they are not classical nodes. This is mostly used internally to ensure that the generation of nodes and networks is processed in the right order.
  }
}
\details{
An adjacency matrix is simply a square matrix in which each node has one column and one row associated with it. For example, if the node A has a causal effect on node B, the matrix will contain \code{1} in the spot \code{matrix["A", "B"]}.

If a time-varying node is also defined as a time-fixed node, the parents of both parts will be pooled when creating the output matrix.
}
\author{
Robin Denz
}
\value{
Returns a numeric square matrix with one row and one column per used node in \code{dag}.
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

# get adjacency matrix
dag2matrix(dag)

# get adjacency matrix using only the child nodes
dag2matrix(dag, include_root_nodes=FALSE)

## adding time-varying nodes
dag <- dag +
  node_td("disease", type="time_to_event", parents=c("age", "smoking"),
          prob_fun=0.01) +
  node_td("cve", type="time_to_event", parents=c("age", "sex", "smoking",
                                                 "disease"),
          prob_fun=0.001, event_duration=Inf)

# get adjacency matrix including all nodes
dag2matrix(dag, include_td_nodes=TRUE)

# get adjacency matrix including only time-constant nodes
dag2matrix(dag, include_td_nodes=FALSE)

# get adjacency matrix using only the child nodes
dag2matrix(dag, include_root_nodes=FALSE)
}
