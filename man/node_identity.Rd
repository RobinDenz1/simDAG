\name{node_identity}
\alias{node_identity}

\title{
Simulate a Node based on an expression
}
\description{
This node type may be used to generate a new node given a regular R expression that may include function calls or any other valid R syntax. This may be useful to combine components of a node which need to be simulated with separate \code{\link{node}} calls, or just as a convenient shorthand for some variable transformations.
}
\usage{
node_identity(data, parents, formula)
}
\arguments{
  \item{data}{
A \code{data.table} (or something that can be coerced to a \code{data.table}) containing all columns specified by \code{parents}.
  }
  \item{parents}{
A character vector specifying the names of the parents that this particular child node has. When using this function as a node type in \code{\link{node}} or \code{\link{node_td}}, this argument usually does not need to be specified because the \code{formula} argument is required and contains all needed information already.
  }
  \item{formula}{
A \code{formula} object containing a \code{~} symbol with nothing on the LHS, and any valid R expression that can be evaluated on \code{data} on the RHS. This expression needs to contain at least one variable name (otherwise users may simply use \code{\link{rconstant}} as node type). It may contain any number of function calls or other valid R syntax, given that all contained objects are included in the global environment. Note that contrary to the other node types supporting this argument, using interactions or levels of a categorical variable are not supported when specifying the formula here.
  }
}
\details{
Custom functions and objects can be used without issues in the \code{formula}, but they need to be present in the global environment, otherwise the underlying \code{eval()} function call will fail. Using this function outside of \code{\link{node}} or \code{\link{node_td}} is essentially equal to using \code{with(data, eval(formula))} (without the \code{~} in the \code{formula}).
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

# define a DAG
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("bmi", type="identity", formula= ~ age + sex + 2)

sim_dat <- sim_from_dag(dag=dag, n_sim=100)
head(sim_dat)

# more complex alternative
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("bmi", type="identity",
       formula= ~ age / 2 + age^2 - ifelse(sex, 2, 3) + 2)

sim_dat <- sim_from_dag(dag=dag, n_sim=100)
head(sim_dat)
}
