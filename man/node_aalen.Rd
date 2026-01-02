\name{node_aalen}
\alias{node_aalen}

\title{
Generate Data from an Aalen Additive Hazards Model
}
\description{
Data from the parents is used to generate the node using Aalen additive hazards regression using the inversion method. Currently, only time-constant coefficients and a constant baseline hazard function are supported.
}
\usage{
node_aalen(data, parents, formula=NULL, betas, intercept,
           cens_dist=NULL, cens_args, name,
           as_two_cols=TRUE, left=0)
}
\arguments{
  \item{data}{
A \code{data.table} (or something that can be coerced to a \code{data.table}) containing all columns specified by \code{parents}.
  }
  \item{parents}{
A character vector specifying the names of the parents that this particular child node has. If non-linear combinations or interaction effects should be included, the user may specify the \code{formula} argument instead.
  }
  \item{formula}{
An optional \code{formula} object to describe how the node should be generated or \code{NULL} (default). This argument only works if the function is used as a node \code{type} in a \code{\link{node}} call. See \code{?node} or the associated vignette for more information about how the \code{formula} argument should be specified in this package.
  }
  \item{betas}{
A numeric vector with length equal to \code{parents}, specifying the causal beta coefficients used to generate the node.
  }
  \item{intercept}{
A single number, specifying the intercept of the model.
  }
  \item{cens_dist}{
A single character naming the distribution function that should be used to generate the censoring times or a suitable function. For example, \code{"runif"} could be used to generate uniformly distributed censoring times. Set to \code{NULL} (default) to get no censoring.
  }
  \item{cens_args}{
A list of named arguments which will be passed to the function specified by the \code{cens_dist} argument.
  }
  \item{name}{
A single character string specifying the name of the node.
  }
  \item{as_two_cols}{
Either \code{TRUE} or \code{FALSE}, specifying whether the output should be divided into two columns. When \code{cens_dist} is specified, this argument will always be treated as \code{TRUE} because two columns are needed to encode both the time to the event and the status indicator. When no censoring is applied, however, users may set this argument to \code{FALSE} to simply return the numbers as they are.
  }
  \item{left}{
A single number, specifying the left-truncation time. If set to something > 0, only times that are larger than this value will be generated. Is set to 0 by default, so that no left-truncation is used.
  }
}
\details{
This function generates survival times according to a Aalen additive hazards model with time-constant beta coefficients and a time-constant baseline hazard. Time-dependent effects or time-dependent baseline hazards are currently not supported. To also include censoring, this function allows the user to supply a function that generates random censoring times. If the censoring time is smaller than the generated survival time, the individual is considered censored.

Like the other time-to-event based \code{\link{node}} type functions, this function usually adds \strong{two} columns to the resulting dataset instead of one. The first column is called \code{paste0(name, "_status")} and is a logical variable, where \code{TRUE} indicates that the event has happened and \code{FALSE} indicates right-censoring. The second column is named \code{paste0(name, "_time")} and includes the survival or censoring time corresponding to the previously mentioned event indicator. This is the standard format for right-censored time-to-event data without time-varying covariates. If no censoring is applied, this behavior can be turned off using the \code{as_two_cols} argument.
}
\references{
Aalen, Odd O. A Linear Regression Model for the Analysis of Life Times. Statistics in Medicine. 1989; (8): 907-925.
}
\author{
Robin Denz
}
\value{
Returns a \code{data.table} of length \code{nrow(data)} containing two columns if \code{as_two_cols=TRUE} and always when \code{cens_dist} is specified. In this case, both columns start with the nodes \code{name} and end with \code{_status} and \code{_time}. The first is a logical vector, the second a numeric one. If \code{as_two_cols=FALSE} and \code{cens_dist} is \code{NULL}, a numeric vector is returned instead.
}
\examples{
library(simDAG)

set.seed(34543)

# define DAG, here with two baseline covariates and
# no censoring of Y
dag <- empty_dag() +
  node("A", type="runif") +
  node("B", type="rbernoulli") +
  node("Y", type="aalen", formula= ~ 0.1 + A*0.2 + B*-0.05)

sim_dat <- sim_from_dag(dag=dag, n_sim=1000)
head(sim_dat)
}
