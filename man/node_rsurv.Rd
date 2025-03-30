\name{node_rsurv}
\alias{node_rsurv}
\alias{node_aftreg}
\alias{node_ahreg}
\alias{node_ehreg}
\alias{node_ypreg}
\alias{node_poreg}

\title{
Simulate a Node Using Parametric Survival Models
}
\description{
Data from the parents is used to generate the node using either an accelerated failure time model, an accelerated hazard model, an extended hazard model, a proportional odds model or a Young and Prentice model, as implemented in the \pkg{rsurv} package (Demarqui 2024).
}
\usage{
node_aftreg(data, parents, betas, baseline, dist=NULL,
            package=NULL, u=stats::runif(nrow(data)),
            cens_dist=NULL, cens_args, name, as_two_cols=TRUE,
            ...)

node_ahreg(data, parents, betas, baseline, dist=NULL,
           package=NULL, u=stats::runif(nrow(data)),
           cens_dist=NULL, cens_args, name, as_two_cols=TRUE,
           ...)

node_ehreg(data, parents, betas, phi, baseline, dist=NULL,
           package=NULL, u=stats::runif(nrow(data)),
           cens_dist=NULL, cens_args, name, as_two_cols=TRUE,
           ...)

node_poreg(data, parents, betas, baseline, dist=NULL,
           package=NULL, u=stats::runif(nrow(data)),
           cens_dist=NULL, cens_args, name, as_two_cols=TRUE,
           ...)

node_ypreg(data, parents, betas, phi, baseline, dist=NULL,
           package=NULL, u=stats::runif(nrow(data)),
           cens_dist=NULL, cens_args, name, as_two_cols=TRUE,
           ...)
}
\arguments{
  \item{data}{
A \code{data.table} (or something that can be coerced to a \code{data.table}) containing all columns specified by \code{parents}. Passed to the argument of the same name in \code{\link[rsurv]{raftreg}}, \code{\link[rsurv]{rahreg}}, \code{\link[rsurv]{rehreg}}, \code{\link[rsurv]{rporeg}} or \code{\link[rsurv]{rypreg}}.
  }
  \item{parents}{
A character vector specifying the names of the parents that this particular child node has. Converted into a formula and passed to the argument of the same name in \code{\link[rsurv]{raftreg}}, \code{\link[rsurv]{rahreg}}, \code{\link[rsurv]{rehreg}}, \code{\link[rsurv]{rporeg}} or \code{\link[rsurv]{rypreg}}.
  }
  \item{betas}{
A numeric vector with length equal to \code{parents}, specifying the causal beta coefficients used to generate the node. Passed to the \code{beta} argument in \code{\link[rsurv]{raftreg}}, \code{\link[rsurv]{rahreg}}, \code{\link[rsurv]{rehreg}}, \code{\link[rsurv]{rporeg}} or \code{\link[rsurv]{rypreg}}.
  }
  \item{phi}{
A numeric vector with length equal to \code{parents}, specifying the phi beta coefficients used to generate the node. Only required for extended hazard and Yang and Prentice models. Passed to the \code{phi} argument in \code{\link[rsurv]{rehreg}} or \code{\link[rsurv]{rypreg}}.
  }
  \item{baseline}{
A single character string, specifying the name of the baseline survival distribution. Passed to the argument of the same name in \code{\link[rsurv]{raftreg}}, \code{\link[rsurv]{rahreg}}, \code{\link[rsurv]{rehreg}}, \code{\link[rsurv]{rporeg}} or \code{\link[rsurv]{rypreg}}.
  }
  \item{dist}{
An alternative way to specify the baseline survival distribution. Passed to the argument of the same name in \code{\link[rsurv]{raftreg}}, \code{\link[rsurv]{rahreg}}, \code{\link[rsurv]{rehreg}}, \code{\link[rsurv]{rporeg}} or \code{\link[rsurv]{rypreg}}.
  }
  \item{package}{
A single character string, specifying the name of the package where the assumed quantile function is implemented. Passed to the argument of the same name in \code{\link[rsurv]{raftreg}}, \code{\link[rsurv]{rahreg}}, \code{\link[rsurv]{rehreg}}, \code{\link[rsurv]{rporeg}} or \code{\link[rsurv]{rypreg}}.
  }
  \item{u}{
A numeric vector of quantiles of length \code{nrow(data)}. Usually this should simply be passed a vector of randomly generated uniformly distributed values between 0 and 1, as defined by the default. Passed to the argument of the same name in \code{\link[rsurv]{raftreg}}, \code{\link[rsurv]{rahreg}}, \code{\link[rsurv]{rehreg}}, \code{\link[rsurv]{rporeg}} or \code{\link[rsurv]{rypreg}}.
  }
  \item{cens_dist}{
A single character naming the distribution function that should be used to generate the censoring times or a suitable function. For example, \code{"runif"} could be used to generate uniformly distributed censoring times. Set to \code{NULL} to get no censoring.
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
  \item{...}{
Further arguments passed to \code{\link[rsurv]{raftreg}}, \code{\link[rsurv]{rahreg}}, \code{\link[rsurv]{rehreg}}, \code{\link[rsurv]{rporeg}} or \code{\link[rsurv]{rypreg}}.
  }
}
\details{
Survival times are generated according to the specified parametric survival model. The actual generation of the values is done entirely by calls to the \pkg{rsurv} package. All arguments are directly passed to the corresponding function in \pkg{rsurv}. Only the censoring is added on top of it. These convenience wrappers only exist to allow direct integration of these data generation functions with the interface provided by \pkg{simDAG}. Please consult the documentation and excellent paper by Demarqui (2024) for more information about the models and how to specify the arguments.
}
\references{
Demarqui Fabio N. Simulation of Survival Data with the Package rsurv. (2024) arXiv:2406.01750v1.
}
\author{
Robin Denz
}
\value{
Returns a \code{data.table} of length \code{nrow(data)} containing two columns if \code{as_two_cols=TRUE} and always when \code{cens_dist} is specified. In this case, both columns start with the nodes \code{name} and end with \code{_event} and \code{_time}. The first is a logical vector, the second a numeric one. If \code{as_two_cols=FALSE} and \code{cens_dist} is \code{NULL}, a numeric vector is returned instead.
}
\examples{
library(simDAG)

set.seed(3454)

if (requireNamespace("rsurv")) {

library(rsurv)

# accelerated failure time model
dag <- empty_dag() +
  node(c("A", "B", "C"), type="rnorm", mean=0, sd=1) +
  node("Y", type="aftreg", formula= ~ -2 + A*0.2 + B*0.1 + A:B*1,
       baseline="weibull", shape=1, scale=2)
data <- sim_from_dag(dag, n_sim=100)

# accelerated hazard model
dag <- empty_dag() +
  node(c("A", "B", "C"), type="rnorm", mean=0, sd=1) +
  node("Y", type="ahreg", formula= ~ -2 + A*0.2 + B*0.1,
       baseline="weibull", shape=1, scale=2)
data <- sim_from_dag(dag, n_sim=100)

# extended hazard model
dag <- empty_dag() +
  node(c("A", "B", "C"), type="rnorm", mean=0, sd=1) +
  node("Y", type="ehreg", formula= ~ -2 + A*0.2 + B*0.1,
       baseline="weibull", shape=1, scale=2,
       phi=c(-1, 1))
data <- sim_from_dag(dag, n_sim=100)

# proportional odds model
dag <- empty_dag() +
  node(c("A", "B", "C"), type="rnorm", mean=0, sd=1) +
  node("Y", type="poreg", formula= ~ -2 + A*0.2 + B*0.1,
       baseline="weibull", shape=1, scale=2)
data <- sim_from_dag(dag, n_sim=100)

# Young and Prentice model
dag <- empty_dag() +
  node(c("A", "B", "C"), type="rnorm", mean=0, sd=1) +
  node("Y", type="ypreg", formula= ~ -2 + A*0.2 + B*0.1,
       baseline="weibull", shape=1, scale=2,
       phi=c(-1, 1))
data <- sim_from_dag(dag, n_sim=100)
}
}
