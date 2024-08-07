\name{node_poisson}
\alias{node_poisson}

\title{
Simulate a Node Using Poisson Regression
}
\description{
Data from the parents is used to generate the node using poisson regression by predicting the covariate specific lambda and sampling from a poisson distribution accordingly.
}
\usage{
node_poisson(data, parents, formula=NULL, betas, intercept)
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
  \item{intercept}{
A single number specifying the intercept that should be used when generating the node.
  }
}
\details{
Essentially, this function simply calculates the linear predictor defined by the \code{betas}-coefficients, the \code{intercept} and the values of the \code{parents}. The exponential function is then applied to this predictor and the result is passed to the \code{\link[stats]{rpois}} function. The result is a draw from a subject-specific poisson distribution, resembling the user-defined poisson regression model.

\strong{\emph{Formal Description}}:

Formally, the data generation can be described as:

\deqn{Y \sim Poisson(\lambda),}

where \eqn{Poisson()} means that the variable is Poisson distributed with:

\deqn{P_\lambda(k) = \frac{\lambda^k e^{-\lambda}}{k!}.}

Here, \eqn{k} is the count and \eqn{e} is eulers number. The parameter \eqn{\lambda} is determined as:

\deqn{\lambda = \exp(\texttt{intercept} + \texttt{parents}_1 \cdot \texttt{betas}_1 + ... + \texttt{parents}_n \cdot \texttt{betas}_n),}

where \eqn{n} is the number of parents (\code{length(parents)}).

For example, given \code{intercept=-15}, \code{parents=c("A", "B")}, \code{betas=c(0.2, 1.3)} the data generation process is defined as:

\deqn{Y \sim Poisson(\exp(-15 + A \cdot 0.2 + B \cdot 1.3)).}
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

set.seed(345345)

dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("smoking", type="poisson",
       formula= ~ -2 + sexTRUE*1.1 + age*0.4)

sim_dat <- sim_from_dag(dag=dag, n_sim=100)
}
