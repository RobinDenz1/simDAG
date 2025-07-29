\name{network}
\alias{network}
\alias{network_td}

\title{
Create a network object for a \code{DAG}
}
\description{
These functions (in conjunction with the \code{\link{empty_dag}} and \code{\link{node}} functions) allow users to create \code{DAG} objects with one or more, possibly time-varying, network structures. This makes it possible to simulate data with complex dependencies among observations using the \code{\link{sim_from_dag}} function or the \code{\link{sim_discrete_time}} function.
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



}
\value{
Returns a \code{DAG.network} object which can be added to a \code{DAG} object directly.
}
\author{
Robin Denz
}
\examples{

}
