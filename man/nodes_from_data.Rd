\name{nodes_from_data}
\alias{nodes_from_data}

\title{
Create a List of Fully Specified Nodes From Data
}
\description{
Given a list of nodes, where only the name, type and the parents are specified plus a \code{data.frame} containing realizations of these nodes, return a list of fully specified nodes (with beta-coefficients, intercepts, errors, ...). The returned list can be used directly to simulate data with the \code{\link{sim_from_dag}} function.
}
\usage{
nodes_from_data(data, nodes, return_models=FALSE, na.rm=FALSE)
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
It can be cumbersone to specify all the node information needed for the simulation, especially when there are a lot of nodes to consider. Additionally, if data is available, it is natural to fit appropriate models to the data to get an empirical estimate of the node information for the simulation. This function automates this process.

All the user has to do is create a list of \code{nodes} including information on the \code{parents}, the \code{name} and the node \code{type}. For root nodes, the required distribution parameters are extracted from the data. For child nodes, regression models corresponding to the specified \code{type} are fit to the data using the \code{parents} as independent covariates and the \code{name} as dependent variable. All required information is extracted from these models and added to the respective node. The output contains lists for the \code{root_nodes} and \code{child_nodes} which can be used directly in the \code{\link{sim_from_dag}} function. It may also include the fit models for further inspection, if \code{return_models=TRUE}.

If the user has a reasonable DAG and knows the node types, this is a very fast way to generate synthetic data that corresponds well to the empirical data. Missing values, however, are ignored and not simulated.
}
\author{
Robin Denz
}
\examples{
\dontrun{
# TODO: add examples
}
}
