\name{empty_dag}
\alias{empty_dag}

\title{
Initialize an empty \code{DAG} object
}
\description{
This function should be used in conjunction with multiple calls to \code{\link{node}} or \code{\link{node_td}} to create a \code{DAG} object, which can then be used to simulate data using the \code{\link{sim_from_dag}} and \code{\link{sim_discrete_time}} functions.
}
\usage{
empty_dag()
}
\details{
Note that this function is only used to initialize an empty \code{DAG} object. Actual information about the respective nodes have to be added using the \code{\link{node}} function or the \code{\link{node_td}} function. The documentation page of that function contains more information on how to correctly do this.
}
\value{
Returns an empty \code{DAG} object.
}
\author{
Robin Denz
}
\examples{
library(simDAG)

# just an empty DAG
empty_dag()

# adding a node to it
empty_dag() + node("age", type="rnorm", mean=20, sd=5)
}
