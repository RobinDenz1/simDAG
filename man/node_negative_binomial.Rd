\name{node_negative_binomial}
\alias{node_negative_binomial}

\title{
Generate Data from a Negative Binomial Regression Model
}
\description{
Data from the parents is used to generate the node using negative binomial regression by applying the betas to the design matrix and sampling from the \code{rnbinom} function.
}
\usage{
node_negative_binomial(data, parents, formula=NULL, betas,
                       intercept, theta)
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
  \item{intercept}{
A single number specifying the intercept that should be used when generating the node.
  }
  \item{theta}{
A single number specifying the theta parameter (\code{size} argument in \code{rnbinom}).
  }
}
\details{
This function uses the linear predictor defined by the \code{betas} and the input design matrix to sample from a subject-specific negative binomial distribution. It does to by calculating the linear predictor using the \code{data}, \code{betas} and \code{intercept}, exponentiating it and passing it to the \code{mu} argument of the \code{rnbinom} function of the \pkg{stats} package.

This node type currently does not support inclusion of random effects or random slopes in the \code{formula}.
}
\author{
Robin Denz
}
\value{
Returns a numeric vector of length \code{nrow(data)}.
}
\seealso{
\code{\link{empty_dag}}, \code{\link{node}}, \code{\link{node_td}}, \code{\link{sim_from_dag}}, \code{\link{sim_discrete_time}}
}
\examples{
library(simDAG)

set.seed(124554)

dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("smoking", type="negative_binomial", theta=0.05,
       formula= ~ -2 + sexTRUE*1.1 + age*0.4)

sim_dat <- sim_from_dag(dag=dag, n_sim=100, sort_dag=FALSE)
}
