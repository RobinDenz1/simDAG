\name{sim_from_dag}
\alias{sim_from_dag}

\title{
Simulate Data from a Given DAG and Node Information
}
\description{
This function can be used to generate data from a given DAG. It additionally requires information on node distributions, beta coefficients and, depending on the node type, more parameters such as intercepts.
}
\usage{
sim_from_dag(dag, n_sim, sort_dag=TRUE, check_inputs=TRUE)
}
\arguments{
  \item{dag}{
A \code{DAG} object created using the \code{\link{empty_dag}} function with nodes added to it using the \code{+} syntax. See details.
  }
  \item{n_sim}{
A single number specifying how many observations should be generated.
  }
  \item{sort_dag}{
Whether to topologically sort the DAG before starting the simulation or not. If the nodes in \code{child_nodes} are already topologically sorted, this argument can be set to \code{FALSE} to safe some computation time. This usually won't safe too much time though, because it internally uses the \code{topological_sort} function from the \pkg{Rfast} package, which is very fast.
  }
  \item{check_inputs}{
Whether to perform plausibility checks for the user input or not. Is set to \code{TRUE} by default, but can be set to \code{FALSE} in order to speed things up when using this function in a simulation study or something similar.
  }
}
\details{

\strong{\emph{How it Works}}:

First, \code{n_sim} i.i.d. samples from the root nodes are drawn. Children of these nodes are then generated one by one according to specified relationships and causal coefficients. For example, lets suppose there are two root nodes, \code{age} and \code{sex}. Those are generated from a normal distribution and a bernoulli distribution respectively. Afterward, the child node \code{height} is generated using both of these variables as parents according to a linear regression with defined coefficients, intercept and sigma (random error). This works because every DAG has at least one topological ordering, which is a linear ordering of vertices such that for every directed edge u v, vertex u comes before v in the ordering. By using \code{sort_dag=TRUE} it is ensured that the nodes are processed in such an ordering.

This procedure is simple in theory, but can get very complex when manually coded. This function offers a simplified workflow by only requiring the user to define the \code{dag} object with appropriate information (see documentation of \code{\link{node}} function). A sample of size \code{n_sim} is then generated from the DAG specified by those two arguments.

\strong{\emph{Specifying the DAG}}:

Concrete details on how to specify the needed \code{dag} object are given in the documentation page of the \code{\link{node}} function and in the vignettes of this package.

\strong{\emph{Can this function create longitudinal data?}}

Yes it can, but only if the user-specified \code{dag} directly specifies a node for each desired point in time. Using the \code{sim_discrete_time} is better in some cases. A brief discussion about this topic can be found in the vignettes of this package.

}
\author{
Robin Denz
}
\examples{
library(simDAG)

set.seed(345345)

dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("bmi", type="gaussian", parents=c("sex", "age"),
       betas=c(1.1, 0.4), intercept=12, error=2)

sim_dat <- sim_from_dag(dag=dag, n_sim=1000)

# More examples for each directly supported node type as well as for custom
# nodes can be found in the documentation page of the respective node function
}
