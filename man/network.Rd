\name{network}
\alias{network}
\alias{network_td}

\title{
Create a network object for a \code{DAG}
}
\description{
These functions (in conjunction with the \code{\link{empty_dag}} and \code{\link{node}} functions) allow users to create \code{DAG} objects with one or more, possibly time-varying, network structures linking individual observations to each other. This makes it possible to simulate data with complex network-based dependencies among observations using the \code{\link{sim_from_dag}} function or the \code{\link{sim_discrete_time}} function.
}
\usage{
network(name, net, ...)

network_td(name, net, create_at_t0=TRUE, ...)
}
\arguments{
  \item{name}{
A single character string, specifying the name of the network. Contrary to the \code{\link{node}} function, multiple values are not allowed, because defining the same network multiple times does not make sense.
  }
  \item{net}{
For \code{network()}, two kinds of inputs are allowed. The first is an \code{igraph} object containing one vertex per observation (e.g. \code{n_sim} vertices) that should be generated when later calling \code{\link{sim_from_dag}} or \code{\link{sim_discrete_time}}. The second is a function that generates such an object, given a named argument called \code{n_sim} and any number of further named arguments. For \code{network_td()}, only the latter kind of input is allowed. Further information is given in the details section.
  }
  \item{create_at_t0}{
Either \code{TRUE} or \code{FALSE}, specifying whether the network should be generated at time 0 in discrete-time simulations (e.g. when other time-independent nodes and networks are generated) or only after the creation of data time 0. Defaults to \code{TRUE}.
  }
  \item{...}{
Optional further named arguments passed to \code{net} if it is a function.
  }
}
\details{

\strong{\emph{What does it mean to add a network to a \code{DAG}?}}

When using only \code{\link{node}} or \code{\link{node_td}} to define a \code{DAG}, all observations are usually generated independently from each other (if not explicitly done otherwise using a custom node function). This reflects the classic i.i.d. assumption that is frequently used everywhere. For some data generation processes, however, this assumption is insufficient. The spread of an infectious disease is a classic example.

The \code{network()} function allows users to relax this assumption, by making it possible to define one or more networks that can then be added to \code{DAG} objects using the \code{+} syntax. These networks should contain a single vertex for each observation that should be generated, placing each row of the dataset into one place in the network. Through the use of the \code{\link{net}} function it is then possible to define new nodes as a function of the neighbors of an observation, where the neighbors of a vertex are defined as any other vertex that is directly connected to this node. For example, one could use this capability to use the mean age of an observations neighbors in a regression model, or use the number of infected neighbors to model the probability of infection. By combining this network-simulation approach with the already extensive simulation capabilities of \code{DAG} based simulations, almost any DGP can be modelled. This approach is described more rigorously in the excellent paper given by Sofrygin et al. (2017).

\strong{\emph{Supported network types}}:

Users may add any number of networks to a \code{DAG} object, making it possible to embed individuals in multiple distinct networks at the same time. These networks can then be used simultaneously to define a single or multiple (possibly time-varying) nodes, using multiple \code{\link{net}} function calls in the respective \code{formula} arguments. It is also possible to define time-varying or dynamic networks that change over time, possibly as a function of the generated data, simulation time or previous states of the network. Examples are given below and in the associated vignette.

Currently, only undirected networks that include exactly one vertex per observation are supported in this package. Future versions might add support for more network types. Additionally, the current syntax only allows aggregation of the values of direct neighbors of an observation, it does not allow using extended neighborhoods.

\strong{\emph{Weighted Networks}}:

It is possible to supply weighted networks to \code{network()}. The weights are then also stored and available to the user when using the \code{\link{net}} function through the internal \code{..weight..} variable. For example, if a weighted network was supplied, the following would be valid syntax: \code{net(weighted.mean(A, ..weight..))} (assuming that \code{A} is a previously defined variable). Note that the \code{..weight..} must be used explicitly, otherwise the weights are ignored.

\strong{\emph{Computational considerations}}:

Including \code{net()} terms in a node might significantly increase the amount of RAM used and the required computation time, especially with very large networks and / or large values of \code{n_sim} and / or \code{max_t} (the latter is only relevant in discrete-time simulations using \code{\link{sim_discrete_time}}). The reason for this is that each time a node is generated or updated over time, the mapping of individuals to their neighbors' values plus the subsequent aggregation has to be performed, which required \code{merge()} calls etc. Usually this should not be a problem, but it might be for some large discrete-time simulations. If the same \code{\link{net}} call is used in multiple nodes it can be beneficial to put it into an extra \code{\link{node}} call and safe it to avoid re-calculating the same thing over and over again (see examples).

\strong{\emph{Further information}}:

For a theoretical treatment, please consult the paper by Sofrygin et al. (2017), who also describe their slightly different implementation of this method in the \pkg{simcausal} package. More information on how to specify network-based dependencies in a \code{DAG} (using \pkg{simDAG}) after adding a network, please consult the \code{\link{net}} documentation page or the associated vignette.

}
\value{
Returns a \code{DAG.network} object which can be added to a \code{DAG} object directly.
}
\references{
Sofrygin, Oleg, Romain Neugebauer and Mark J. van der Laan (2017). Conducting Simulations in Causal Inference with Networks-Based Structural Equation Models. arXiv preprint, doi: 10.48550/arXiv.1705.10376
}
\author{
Robin Denz
}
\examples{
library(igraph)
library(data.table)
library(simDAG)

set.seed(2368)

# generate random undirected / unweighted networks as examples
g1 <- igraph::sample_gnm(n=20, m=30)
g2 <- igraph::sample_gnm(n=20, m=30)

# adding a single network to a DAG, with Y being dependent on
# the mean value of A of its neighbors
dag <- empty_dag() +
  network("Net1", net=g1) +
  node("A", type="rnorm") +
  node("Y", type="gaussian", formula= ~ -2 + net(mean(A))*1.3, error=1.5)

# NOTE: because we supplied the network of size 20 directly, we can only
#       use n_sim=20 here
data <- sim_from_dag(dag, n_sim=20)

# using multiple networks, with Y being differently dependent on
# the mean value of A of its neighbors in both networks
dag <- empty_dag() +
  network("Net1", net=g1) +
  network("Net2", net=g2) +
  node("A", type="rnorm") +
  node("Y", type="gaussian", formula= ~ -2 + net(mean(A), net="Net1")*1.3 +
        net(mean(A), net="Net2")*-2, error=1.5)

# using a function to add networks, to allow any value of 'n_sim' later

# exemplary function that returns a random network of size 'n_sim'
gen_network <- function(n_sim) {
  igraph::sample_gnm(n=n_sim, m=30)
}

# same as first example, but using the function as input
dag <- empty_dag() +
  network("Net1", net=gen_network) +
  node("A", type="rnorm") +
  node("Y", type="gaussian", formula= ~ -2 + net(mean(A))*1.3, error=1.5)
data <- sim_from_dag(dag, n_sim=25)
}
