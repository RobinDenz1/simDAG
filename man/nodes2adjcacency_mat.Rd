\name{nodes2adjacency_mat}
\alias{nodes2adjacency_mat}

\title{
Create a List of Fully Specified Nodes From Data
}
\description{
The \code{\link{sim_from_dag}} function requires the user to specify the causal relationships by using two lists of further list containing node information. This function takes these node lists as input and outputs the underlying adjacency matrix. This can be usefull to plot the theoretical DAG or to check if the nodes have been specified correctly.
}
\usage{
nodes2adjacency_mat(child_nodes, root_nodes=NULL)
}
\arguments{
  \item{child_nodes}{
A list of child nodes as described in the \code{\link{sim_from_dag}} function.
  }
  \item{root_nodes}{
A list of root nodes as described in the \code{\link{sim_from_dag}} function or NULL.
  }
}
\details{
An adjacency matrix is simply a square matrix in which each node has one column and one row associated with it. For example, if the node A has a causal effect on node B, the matrix will contain \code{TRUE} in the spot \code{matrix["A", "B"]}.
}
\examples{
root_nodes <- list(list(dist="rnorm",
                        params=list(mean=50, sd=4),
                        name="age"),
                   list(dist="rbernoulli",
                        params=list(p=0.5),
                        name="sex"))
child_nodes <- list(list(parents=c("sex", "age"),
                         type="gaussian",
                         name="bmi",
                         betas=c(1.1, 0.4),
                         intercept=12,
                         error=2),
                    list(parents=c("age", "bmi"),
                         type="binomial",
                         name="death",
                         betas=c(0.1, 0.3),
                         intercept=-15))

nodes2adjacency_mat(root_nodes=root_nodes, child_nodes=child_nodes)
}
