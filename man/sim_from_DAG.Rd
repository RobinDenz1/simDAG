\name{sim_from_dag}
\alias{sim_from_dag}

\title{
Simulate Data from a \code{DAG}
}
\description{
This function can be used to generate data from a given \code{DAG}. The \code{DAG} should be created using the \code{\link{empty_dag}} and \code{\link{node}} functions, which require the user to fully specify all variables, including information about distributions, beta coefficients and, depending on the node type, more parameters such as intercepts. Network dependencies among observations may also be included using the \code{\link{network}} function.
}
\usage{
sim_from_dag(dag, n_sim, sort_dag=FALSE, return_networks=FALSE,
             check_inputs=TRUE)
}
\arguments{
  \item{dag}{
A \code{DAG} object created using the \code{\link{empty_dag}} function with \code{\link{node}} calls (and potentially \code{\link{network}} calls) added to it using the \code{+} syntax. See details.
  }
  \item{n_sim}{
A single number specifying how many observations should be generated.
  }
  \item{sort_dag}{
Whether to topologically sort the DAG before starting the simulation or not. If the nodes in \code{dag} were already added in a topologically sorted manner, this argument can be kept at \code{FALSE}. It is recommended to not rely on this argument too heavily, because sorting may sometimes fail when only a \code{formula} is supplied to one or more \code{\link{node}} calls.
  }
  \item{return_networks}{
Whether to also return networks that were included or generated due to the presence of \code{\link{network}} calls in the supplied \code{dag} or not. If set to \code{TRUE}, a named list of length 2 will be returned instead of only returning the generated data. Defaults to \code{FALSE}.
  }
  \item{check_inputs}{
Whether to perform plausibility checks for the user input or not. Is set to \code{TRUE} by default, but can be set to \code{FALSE} in order to speed things up when using this function in a simulation study or something similar.
  }
}
\details{

\strong{\emph{How it Works}}:

First, \code{n_sim} i.i.d. samples from the root nodes are drawn. Children of these nodes are then generated one by one according to specified relationships and causal coefficients. For example, lets suppose there are two root nodes, \code{age} and \code{sex}. Those are generated from a normal distribution and a bernoulli distribution respectively. Afterward, the child node \code{height} is generated using both of these variables as parents according to a linear regression with defined coefficients, intercept and sigma (random error). This works because every DAG has at least one topological ordering, which is a linear ordering of vertices such that for every directed edge \eqn{u} \eqn{v}, vertex \eqn{u} comes before \eqn{v} in the ordering. By using \code{sort_dag=TRUE} it is ensured that the nodes are processed in such an ordering.

This procedure is simple in theory, but can get very complex when manually coded. This function offers a simplified workflow by only requiring the user to define the \code{dag} object with appropriate information (see documentation of \code{\link{node}} function). A sample of size \code{n_sim} is then generated from the DAG specified by those two arguments.

\strong{\emph{Specifying the DAG}}:

Concrete details on how to specify the needed \code{dag} object are given in the documentation page of the \code{\link{node}} and \code{\link{network}} functions and in the vignettes of this package.

\strong{\emph{Can this function create longitudinal data?}}

Yes and no. It theoretically can, but only if the user-specified \code{dag} directly specifies a node for each desired point in time. Using the \code{\link{sim_discrete_time}} is better in some cases. A brief discussion about this topic can be found in the vignettes of this package.

If time-dependent nodes were added to the \code{dag} using \code{\link{node_td}} calls, this function may not be used. Only the \code{\link{sim_discrete_time}} function will work in that case.

\strong{\emph{Networks-Based Simulation}}

In some cases the assumption that observations (rows) are independent from each other is not sufficient. This function allows to relax this assumption by directly supporting network-based dependencies among individuals. Users may specify one or multiple networks of dependencies between individuals and add those to the \code{dag} using the \code{\link{network}} function. It is then possible to use the \code{\link{net}} function inside the \code{formula} argument of \code{\link{node}} calls to directly make the value of that node dependent on some other variable values of its' neighbors in the network. See the documentation and the associated vignette for more information.

}
\author{
Robin Denz
}
\value{
If \code{return_networks=FALSE}, returns a single \code{data.table} including the simulated data with (at least) one column per node specified in \code{dag} and \code{n_sim} rows. Otherwise it returns a named list containing the \code{data} and the \code{networks} supplied or generated through the course of the simulation.
}
\seealso{
\code{\link{empty_dag}}, \code{\link{node}}, \code{\link{network}}, \code{\link{plot.DAG}}, \code{\link{sim_discrete_time}}
}
\references{
Denz, Robin and Nina Timmesfeld (2025). Simulating Complex Crossectional and Longitudinal Data using the simDAG R Package. arXiv preprint, doi: 10.48550/arXiv.2506.01498.
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
