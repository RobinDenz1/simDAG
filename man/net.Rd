\name{net}
\alias{net}

\title{
Specify Network Dependencies in a \code{DAG}
}
\description{
This function may be used in the \code{formula} of nodes in which the value of the observation of one individual are dependent on its' neighbors in a defined static \code{\link{network}} or dynamic \code{\link{network_td}}. Given the network and a previously generated variable, \code{net()} aggregates data of the neighbors according to an arbitrary function under the hood. The resulting variable can then be used directly in a \code{formula}.
}
\usage{
net(expr, net=NULL, mode="all", na=NA)
}
\arguments{
  \item{expr}{
Any R expression, usually containing one or more previously generated variables, that returns one numeric value given a vector, such as \code{sum(variable)} or \code{mean(variable)}.
  }
  \item{net}{
A single character string specifying the name of the network that should be used to define the neighbors of an observation. If only one network is present in the \code{DAG}, this argument can be omitted. The single added network is then used by default. If multiple networks are present and this argument is not defined, an error will be produced.
  }
  \item{mode}{
A single character, specifying how to use the direction of the edges if a directed network is supplied (ignored otherwise). If \code{"all"}, the direction of the edges is ignored and both incoming and outgoing edges are used to define the neighbors of each individual. If \code{"out"}, only the individuals who \eqn{i} (the observation row) is pointing to are used as neighbors and if \code{"in"} only the individuals who point to \eqn{i} are being used as neighbors.
  }
  \item{na}{
A single value assigned to the variable if \code{expr} could not be computed. This can happen due to the nature of the expression (e.g. \code{NA} being returned directly after evaluating the expression for some reason), or when an observation does not have any neighbors in a network.
  }
}
\details{

\strong{\emph{How it works}}:

Internally the following procedure is used whenever a \code{net()} function call is included in a \code{formula} of a \code{\link{node}} (regardless of whether time-fixed or time-dependent). First, the associated network (defined using the \code{net} argument) is used to identify the neighbors of each observation. Every vertex that is directly connected to an observation is considered its' neighbor. The parent variable(s) specified in the \code{net()} call are then aggregated over these neighbors using the given \code{expr}. A simple example: consider observation \code{1} with four neighbors named \code{2, 5, 8} and \code{10}. The \code{formula} contains the following \code{net()} call: \code{net(sum(infected))}. The value of the \code{infected} variable is 0, 0, 1, 1 for persons \code{2, 5, 8} and \code{10} respectively. These values are then summed up to result in a value of 2 for person \code{1}. The same is done for every person in the simulated data. The resulting variable is then used as-is in the simulation.

\strong{\emph{Supported inputs}}:

Any function that returns a single (usually numeric) value, given the neighbors' values can be used. It is therefore also possible to make the simulation dependent on specific neighbors only. For example, using \code{infected[1]} instead of \code{sum(infected)} would return a value of 0 for observation 1 in the above example, because person \code{2} is the first neighbor and has a value of 0. Note that the internally used variable named \code{..neighbor..} includes the ids of the neighbors. The entire \code{expr} is evaluated in a \pkg{data.table} call of the form: \code{data[, .(variable = eval(expr)), by=id]}, making it also possible to use any \pkg{data.table} syntax such as \code{\link[data.table]{.N}} (which would return the number of neighbors a person has).

\strong{\emph{Specifying parents}}:

Whenever a \code{net()} call is used in a \code{formula}, we recommend specifying the \code{parents} argument of the node as well. The reason for this recommendation is, that it is sometimes difficult to identify which variables are used in \code{net()} calls, depending on the \code{expr}. This may cause issues if a \code{DAG} is not specified in a topologically sorted manner and users rely on the \code{sort_dag} argument of \code{\link{sim_from_dag}} to re-order the variables. Specifying the \code{parents} ensures that this issue cannot occur.

\strong{\emph{A small warning}}:

Note that it never really makes sense to use this function outside of a \code{formula} argument: if you look at its source code you will realize that it does not actually do anything, except returning its input. It is only a piece of syntax for the \code{formula} interface. Please consult the \code{\link{network}} documentation page or the associated vignette for more information.
}
\value{
"Returns" a numeric vector of length \code{n_sim} when used properly in a \code{\link{sim_from_dag}} or \code{\link{sim_discrete_time}} call. Returns a list of its input when used outside \code{formula}.
}
\author{
Robin Denz
}
\examples{
library(igraph)
library(data.table)
library(simDAG)

# define a random network for illustration, with 10 vertices
set.seed(234)
g <- igraph::sample_smallworld(1, 10, 2, 0.5)

# a simple dag containing only two time-constant variables and the network
dag <- empty_dag() +
  node("A", type="rnorm", mean=0, sd=1) +
  node("B", type="rbernoulli", p=0.5) +
  network("Net1", net=g)

# using the mean of A of each observations neighbor in a linear model
dag2 <- dag +
  node("Y", type="gaussian", formula= ~ -2 + net(mean(A))*4, error=1)

# using an indicator of whether any of an observations neighbors has
# a 1 in B in a linear model
dag3 <- dag +
  node("Y", type="gaussian", formula= ~ 1.5 + net(as.numeric(any(B==1)))*3,
       error=1.2)
}
