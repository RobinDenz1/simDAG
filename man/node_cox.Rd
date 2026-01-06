\name{node_cox}
\alias{node_cox}

\title{
Generate Data from a Cox-Regression Model
}
\description{
Data from the parents is used to generate the node using cox-regression using the method of Bender et al. (2005).
}
\usage{
node_cox(data, parents, formula=NULL, betas, surv_dist, lambda, gamma,
         cens_dist=NULL, cens_args, name, as_two_cols=TRUE,
         left=0)
}
\arguments{
  \item{data}{
A \code{data.table} (or something that can be coerced to a \code{data.table}) containing all columns specified by \code{parents}.
  }
  \item{parents}{
A character vector specifying the names of the parents that this particular child node has. If non-linear combinations or interaction effects should be included, the user may specify the \code{formula} argument instead.
  }
  \item{formula}{
An optional \code{formula} object to describe how the node should be generated or \code{NULL} (default). If supplied it should start with \code{~}, having nothing else on the left hand side. The right hand side may contain any valid formula syntax, such as \code{A + B} or \code{A + B + I(A^2)}, allowing non-linear effects. If this argument is defined, there is no need to define the \code{parents} argument. For example, using \code{parents=c("A", "B")} is equal to using \code{formula= ~ A + B}.
  }
  \item{betas}{
A numeric vector with length equal to \code{parents}, specifying the causal beta coefficients used to generate the node.
  }
  \item{surv_dist}{
A single character specifying the distribution that should be used when generating the survival times. Can be either \code{"weibull"} or \code{"exponential"}.
  }
  \item{lambda}{
A single number used as parameter defined by \code{surv_dist}.
  }
  \item{gamma}{
A single number used as parameter defined by \code{surv_dist}.
  }
  \item{cens_dist}{
A single character naming the distribution function that should be used to generate the censoring times or a suitable function. For example, \code{"runif"} could be used to generate uniformly distributed censoring times. Set to \code{NULL} to get no censoring (default).
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
Either a single number >= 0, or a numeric vector of length \code{nrow(data)} containing only numbers >= 0. When simulating the survival times, only numbers larger than these will be generated using correctly left-truncated sampling. Note that this does not affect the censoring times, only the generated survival times will be left-truncated. Set to 0 (default) to not use left-truncation.
  }
}
\details{
The survival times are generated according to the cox proportional-hazards regression model as defined by the user. How exactly the data-generation works is described in detail in Bender et al. (2005). To also include censoring, this function allows the user to supply a function that generates random censoring times. If the censoring time is smaller than the generated survival time, the individual is considered censored.

Unlike the other \code{\link{node}} type functions, this function usually adds \strong{two} columns to the resulting dataset instead of one. The first column is called \code{paste0(name, "_status")} and is a logical variable, where \code{TRUE} indicates that the event has happened and \code{FALSE} indicates right-censoring. The second column is named \code{paste0(name, "_time")} and includes the survival or censoring time corresponding to the previously mentioned event indicator. This is the standard format for right-censored time-to-event data without time-varying covariates. If no censoring is applied, this behavior can be turned off using the \code{as_two_cols} argument.

To simulate more complex time-to-event data, the user may need to use the \code{\link{sim_discrete_time}} function instead.
}
\note{
This function was updated internally in version 0.5.0 to make it faster and to allow the \code{left} argument. Generating data using this updated version will generally result in different results as compared to earlier versions, even when using the same random number generator seed. To replicate earlier results, please install earlier versions of this package.
}
\references{
Bender R, Augustin T, Blettner M. Generating survival times to simulate Cox proportional hazards models. Statistics in Medicine. 2005; 24 (11): 1713-1723.
}
\author{
Robin Denz
}
\value{
Returns a \code{data.table} of length \code{nrow(data)} containing two columns if \code{as_two_cols=TRUE} and always when \code{cens_dist} is specified. In this case, both columns start with the nodes \code{name} and end with \code{_status} and \code{_time}. The first is a logical vector, the second a numeric one. If \code{as_two_cols=FALSE} and \code{cens_dist} is \code{NULL}, a numeric vector is returned instead.
}
\examples{
library(simDAG)

set.seed(3454)

# define DAG
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("death", type="cox", parents=c("sex", "age"), betas=c(1.1, 0.4),
       surv_dist="weibull", lambda=1.1, gamma=0.7, cens_dist="runif",
       cens_args=list(min=0, max=1))

sim_dat <- sim_from_dag(dag=dag, n_sim=1000)
}
