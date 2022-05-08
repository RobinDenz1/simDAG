\name{nodes_from_data}
\alias{nodes_from_data}

\title{
Create a List of Fully Specified Nodes From Data
}
\description{
Given a list of nodes, where only the name, type and the parents are specified plus a \code{data.frame} containing realizations of these nodes, return a list of fully specified nodes (with beta-coefficients, intercepts, errors, ...). The returned list can be used directly to simulate data with the \code{sim_from_DAG} function.
}
\usage{
nodes_from_data(data, nodes, return_models=FALSE,  na.rm=FALSE)
}
\arguments{
  \item{data}{
A \code{data.frame} containing all columns specified by \code{nodes}.
  }
  \item{nodes}{
A list containing one list per node of interest. Those nodes need to include three parameters each: \code{name} (the name of the node), \code{parents} (a character vector specifying the nodes parents, or \code{NULL} if it is a root node) and \code{type} (one of \code{"gaussian"}, \code{"binomial"}, \code{"poisson"}).
  }
  \item{return_models}{
Whether to return a list of all models that were fit to estimate the information for all child nodes (elements in \code{nodes} where the \code{parents} argument is not \code{NULL}).
  }
  \item{na.rm}{
Whether to remove missing values or not.
  }
}
\details{
More explanation
}
\examples{
\dontrun{

}
}
