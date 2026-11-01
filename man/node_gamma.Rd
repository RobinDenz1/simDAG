\name{node_gamma}
\alias{node_gamma}

\title{
Generate Data from a Gamma Regression Model
}
\description{
Data from the parents is used to generate the node using gamma regression by applying the betas to the design matrix and sampling from the \code{\link[stats]{rgamma}} function.
}
\usage{
node_gamma(data, parents, formula=NULL, betas,
           intercept, shape, link="log")
}
\arguments{
  \item{data}{
A \code{data.table} (or something that can be coerced to a \code{data.table}) containing all columns specified by \code{parents}.
  }
  \item{parents}{
A character vector specifying the names of the parents that this particular child node has. If non-linear combinations or interaction effects should be included, the user may specify the \code{formula} argument instead.
  }
  \item{formula}{
An optional \code{formula} object to describe how the node should be generated or \code{NULL} (default). If supplied it should start with \code{~}, having nothing else on the left hand side. The right hand side may contain any valid formula syntax, such as \code{A + B} or \code{A + B + I(A^2)}, allowing non-linear effects. If this argument is defined, there is no need to define the \code{parents} argument. For example, using \code{parents=c("A", "B")} is equal to using \code{formula= ~ A + B}. Random effects and random slopes are currently not supported here.
  }
  \item{betas}{
A numeric vector with length equal to \code{parents}, specifying the causal beta coefficients used to generate the node.
  }
  \item{intercept}{
A single number specifying the intercept that should be used when generating the node.
  }
  \item{shape}{
A single positive number specifying the shape parameter (\code{shape} argument in \code{\link[stats]{rgamma}}). The shape parameter determines the conditional variance of the generated values relative to their conditional mean.
  }
  \item{link}{
The link function used to transform the linear predictor to the conditional mean used in \code{\link[stats]{rgamma}}. For a standard gamma regression model, this should be set to \code{"log"} (which is the default). Other allowed values are \code{"identity"} and \code{"inverse"}.
  }
}
\details{
This function uses the linear predictor defined by the \code{betas} and the input design matrix to sample from a subject-specific gamma distribution. It does so by calculating the linear predictor using the \code{data}, \code{betas} and \code{intercept}, applying the inverse of the specified link function to obtain the conditional mean, and passing it to the \code{rgamma} function with \code{shape=shape} and \code{scale=mu_i/shape} of the \pkg{stats} package.

For an observation with conditional mean \eqn{\mu_i} and shape parameter \eqn{\alpha}, the generated outcome follows a gamma distribution with shape \eqn{\alpha} and scale \eqn{\mu_i/\alpha}. Consequently, the conditional mean is \eqn{E(Y_i\mid X_i)=\mu_i} and the conditional variance is \eqn{\mathrm{Var}(Y_i\mid X_i)=\mu_i^2/\alpha}.

Every non-missing conditional mean must be finite and strictly positive. This node type currently does not support inclusion of random effects or random slopes in the \code{formula}.
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
  node("cost", type="gamma", shape=3,
       formula= ~ 1.5 + age*0.01 + sexTRUE*0.2)

sim_dat <- sim_from_dag(dag=dag, n_sim=100, sort_dag=FALSE)
head(sim_dat)
}
