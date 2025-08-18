\name{node_zeroinfl}
\alias{node_zeroinfl}

\title{
Generate Data from a Zero-Inflated Count Model
}
\description{
Data from the parents is used to first simulate data for the regular count model, which may follow either a poisson regression or a negative binomial regression, as implemented in \code{\link{node_poisson}} and \code{\link{node_negative_binomial}} respectively. Then, zeros are simulated using a logistic regression model as implemented in \code{\link{node_binomial}}. Whenever the second binomial part returned a 0, the first part is set to 0, leaving the rest untouched. Supports random effects and random slopes (if possible) in both models. See examples.
}
\usage{
node_zeroinfl(data, parents, parents_count,
              parents_zero, formula_count, formula_zero,
              betas_count, betas_zero,
              intercept_count, intercept_zero,
              family_count="poisson", theta,
              var_corr_count, var_corr_zero)
}
\arguments{
  \item{data}{
A \code{data.table} (or something that can be coerced to a \code{data.table}) containing all columns specified by \code{parents}, \code{parents_count} and \code{parents_zero}.
  }
  \item{parents}{
A character vector specifying the names of the parents that this particular child node has. Note that this argument does not have to be specified if \code{parents_count} and \code{parents_zero} are specified. If non-linear combinations or interaction effects should be included, the user should specify the \code{formula_count} and/or \code{formula_zero} arguments instead.
  }
  \item{parents_count}{
Same as \code{parents} but should only contain the parents of the count model part of the node.
  }
  \item{parents_zero}{
Same as \code{parents} but should only contain the parents of the zero-inflation model part of the node.
  }
  \item{formula_count}{
An enhanced formula passed to the \code{\link{node_poisson}} or the \code{\link{node_negative_binomial}} function, used to generate the count part of the node. If this argument is specified, there is no need to specify the \code{parents_count}, \code{betas_count} and \code{intercept_count} arguments. The syntax is the same as in the usual \code{formula} argument as described in \code{\link{node}}.
  }
  \item{formula_zero}{
An enhanced formula passed to the \code{\link{node_binomial}} function, used to generate the zero-inflated part of the node. If this argument is specified, there is no need to specify the \code{parents_zero}, \code{betas_zero} and \code{intercept_zero} arguments. The syntax is the same as in the usual \code{formula} argument as described in \code{\link{node}}.
  }
  \item{betas_count}{
A numeric vector with length equal to \code{parents_count}, specifying the causal beta coefficients used to generate the node in the count model.
  }
  \item{betas_zero}{
A numeric vector with length equal to \code{parents_zero}, specifying the causal beta coefficients used to generate the node in the zero-inflation model.
  }
  \item{intercept_count}{
A single number specifying the intercept that should be used when generating the count model part of the node.
  }
  \item{intercept_zero}{
A single number specifying the intercept that should be used when generating the zero-inflated part of the node.
  }
  \item{family_count}{
Either \code{"poisson"} for a zero-inflated poisson regression or \code{"negative_binomial"} for a zero-inflated negative binomial regression.
  }
  \item{theta}{
A single number specifying the theta parameter (\code{size} argument in \code{rnbinom}). Ignore if \code{family_count="poisson"}.
  }
  \item{var_corr_count}{
If random effects or random slopes are included in \code{formula_count}, this argument should be specified to define the variance structure of these effects. It will be passed to the \code{var_corr} argument of \code{\link{node_poisson}}. Random effects or slopes are currently not supported with \code{family_count="negative_binomial"}.
  }
  \item{var_corr_zero}{
If random effects or random slopes are included in \code{formula_zero}, this argument should be specified to define the variance structure of these effects. It will be passed to the \code{var_corr} argument of \code{\link{node_binomial}}.
  }
}
\details{
It is important to note that data for both underlying models (the count model and the zero-inflation model) are simulated from completely independent of each other. When using random effects in either of the two models, they may therefore use completely different values for each process.
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

set.seed(5425)

# zero-inflated poisson regression
dag <- empty_dag() +
  node(c("A", "B"), type="rnorm", mean=0, sd=1) +
  node("Y", type="zeroinfl",
       formula_count= ~ -2 + A*0.2 + B*0.1 + A:B*0.4,
       formula_zero= ~ 1 + A*1 + B*2,
       family_count="poisson",
       parents=c("A", "B"))
data <- sim_from_dag(dag, n_sim=100)

# above is functionally the same as:
dag <- empty_dag() +
  node(c("A", "B"), type="rnorm", mean=0, sd=1) +
  node("Y_count", type="poisson", formula= ~ -2 + A*0.2 + B*0.1 + A:B*0.4) +
  node("Y_zero", type="binomial", formula= ~ 1 + A*1 + B*2) +
  node("Y", type="identity", formula= ~ Y_zero * Y_count)
data <- sim_from_dag(dag, n_sim=100)

# same as above, but specifying each individual component instead of formulas
dag <- empty_dag() +
  node(c("A", "B", "C"), type="rnorm", mean=0, sd=1) +
  node("Y", type="zeroinfl",
       parents_count=c("A", "B"),
       betas_count=c(0.2, 0.1),
       intercept_count=-2,
       parents_zero=c("A", "B"),
       betas_zero=c(1, 2),
       intercept_zero=1,
       family_count="poisson",
       parents=c("A", "B"))
data <- sim_from_dag(dag, n_sim=100)

# zero-inflated negative-binomial regression
dag <- empty_dag() +
  node(c("A", "B"), type="rnorm", mean=0, sd=1) +
  node("Y", type="zeroinfl",
       formula_count= ~ -2 + A*0.2 + B*3 + A:B*0.4,
       formula_zero= ~ 3 + A*0.1 + B*0.3,
       family_count="negative_binomial", theta=1,
       parents=c("A", "B"))
data <- sim_from_dag(dag, n_sim=100)
}
