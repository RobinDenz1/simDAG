\name{empty_dag}
\alias{empty_dag}

\title{
Initialize an empty \code{DAG} object
}
\description{
This function should be used in conjunction with multiple calls to \code{\link{node}} to create a \code{DAG} object, which can then be used to simulate data using the \code{\link{sim_from_dag}} function.
}
\usage{
empty_dag()
}
\details{
Note that this function is only used to initialize an empty \code{DAG} object. Actual information about the respective nodes have to be added using the \code{\link{node}} function. The documentation page of that function contains more information on how to correctly do this.
}
\value{
Returns an empty \code{DAG} object.
}
\author{
Robin Denz
}
\examples{
## add some examples
}
