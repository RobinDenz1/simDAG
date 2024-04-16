\name{node_gaussian}
\alias{node_gaussian}

\title{
Simulate a Node Using Linear Regression
}
\description{
Data from the parents is used to generate the node using linear regression by predicting the covariate specific mean and sampling from a normal distribution with that mean and a specified standard deviation.
}
\usage{
node_gaussian(data, parents, formula=NULL, betas, intercept, error)
}
\arguments{
  \item{data}{
A \code{data.frame} containing all columns specified by \code{parents}.
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
  \item{intercept}{
A single number specifying the intercept that should be used when generating the node.
  }
  \item{error}{
A single number specifying the sigma error that should be used when generating the node.
  }
}
\details{
Using the general linear regression equation, the observation-specific value that would be expected given the model is generated for every observation in the dataset generated thus far. We could stop here, but this would create a perfect fit for the node, which is unrealistic. Instead, we add an error term by taking one sample of a normal distribution for each observation with mean zero and standard deviation \code{error}. This error term is then added to the predicted mean.

\strong{\emph{Formal Description}}:

Formally, the data generation can be described as:

\deqn{Y \sim \code{intercept} + \code{parents}_1 \cdot \code{betas}_1 + ... + \code{parents}_n \cdot \code{betas}_n+ N(0, \code{error}),}

where \eqn{N(0, \code{error})} denotes the normal distribution with mean 0 and a standard deviation of \code{error} and \eqn{n} is the number of parents (\code{length(parents)}).

For example, given \code{intercept=-15}, \code{parents=c("A", "B")}, \code{betas=c(0.2, 1.3)} and \code{error=2} the data generation process is defined as:

\deqn{Y \sim -15 + A \cdot 0.2 + B \cdot 1.3 + N(0, 2).}

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

set.seed(12455432)

dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("bmi", type="gaussian", parents=c("sex", "age"),
       betas=c(1.1, 0.4), intercept=12, error=2)

sim_dat <- sim_from_dag(dag=dag, n_sim=100)
}
