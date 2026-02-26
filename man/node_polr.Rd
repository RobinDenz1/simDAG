\name{node_polr}
\alias{node_polr}

\title{
Generate Data from an Ordered Logistic or Probit Regression
}
\description{
Data from the parents is used to generate the node using by first calculating the linear predictor. An appropriately distributed random error term is then added. The resulting latent variable is categorized at user-specified cutpoints to obtain the resulting ordered factor.
}
\usage{
node_polr(data, parents, formula=NULL, betas,
          cutpoints, link="logistic", labels=FALSE,
          output="factor")
}
\arguments{
  \item{data}{
A \code{data.table} (or something that can be coerced to a \code{data.table}) containing all columns specified by \code{parents}.
  }
  \item{parents}{
A character vector specifying the names of the parents that this particular child node has. If non-linear combinations or interaction effects should be included, the user may specify the \code{formula} argument instead.
  }
  \item{formula}{
An optional \code{formula} object to describe how the node should be generated or \code{NULL} (default). If supplied it should start with \code{~}, having nothing else on the left hand side. The right hand side may contain any valid formula syntax, such as \code{A + B} or \code{A + B + I(A^2)}, allowing non-linear effects. If this argument is defined, there is no need to define the \code{parents} argument. For example, using \code{parents=c("A", "B")} is equal to using \code{formula= ~ A + B}. Contrary to the \code{\link{node_gaussian}}, \code{\link{node_binomial}} and \code{\link{node_poisson}} node types, random effects and random slopes are currently not supported here.
  }
  \item{betas}{
A numeric vector with length equal to \code{parents}, specifying the causal beta coefficients used to generate the node.
  }
  \item{cutpoints}{
A numeric vector containing the points at which the underlying latent variable is cut to produce the ordered factor. In general, \code{length(cutpoints) + 1} categories will be produced.
  }
  \item{link}{
The link function describing the distribution of the error term. Allowed values are \code{"logistic"}, \code{"probit"}, \code{"loglog"}, \code{"cloglog"} and \code{"cauchit"}, mirroring the \code{method} argument in the \code{\link[MASS]{polr}} function.
  }
  \item{labels}{
Labels for the levels of the resulting categories. By default (\code{labels = FALSE}), simple integer codes are returned. Corresponds to the argument of the same name in the \code{\link{cut}} function.
  }
  \item{output}{
A single character string specifying the kind of output that should be returned. Defaults to \code{"factor"}, but may also be set to \code{"character"}.
  }
}
\details{
This node type currently does not support inclusion of random effects or random slopes in the \code{formula}.
}
\author{
Robin Denz
}
\value{
Returns a character of factor vector of length \code{nrow(data)}.
}
\seealso{
\code{\link{empty_dag}}, \code{\link{node}}, \code{\link{node_td}}, \code{\link{sim_from_dag}}, \code{\link{sim_discrete_time}}
}
\examples{
library(simDAG)

set.seed(124554)

dag <- empty_dag() +
  node(c("X1", "X2"), type="rnorm", mean=0, sd=1) +
  node("Y", type="polr", formula= ~ -2*X1 + 1.5*X2, cutpoints=c(0.7, 1.5, 3.2),
       link="logistic")

sim_dat <- sim_from_dag(dag=dag, n_sim=100)
}
