\name{net}
\alias{net}

\title{
Specify Network Dependencies in a \code{DAG}
}
\description{
This function should be used in the \code{formula} of nodes in which the value of the observation of one individual
}
\usage{
net(expression, net)
}
\arguments{
  \item{name}{
A single character string, specifying the name of the network. Contrary to the \code{\link{node}} function, multiple values are not allowed, because defining the same network multiple times does not make sense.
  }
  \item{net}{
For \code{network()}, two kinds of inputs are allowed. The first is an \code{igraph} object containing one vertex per observation (e.g. \code{n_sim} vertices) that should be generated when later calling \code{\link{sim_from_dag}} or \code{\link{sim_discrete_time}}. The second is a function that generates such an object, given a named argument called \code{n_sim} and any number of further named arguments. For \code{network_td()}, only the latter kind of input is allowed. Further information is given in the details section.
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
