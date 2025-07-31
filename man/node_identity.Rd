\name{node_identity}
\alias{node_identity}

\title{
Generate Data based on an expression
}
\description{
This node type may be used to generate a new node given a regular R expression that may include function calls or any other valid R syntax. This may be useful to combine components of a node which need to be simulated with separate \code{\link{node}} calls, or just as a convenient shorthand for some variable transformations. Also allows calculation of just the linear predictor and generation of intermediary variables using the enhanced \code{formula} syntax.
}
\usage{
node_identity(data, parents, formula, kind="expr",
              betas, intercept, name)
}
\arguments{
  \item{data}{
A \code{data.table} (or something that can be coerced to a \code{data.table}) containing all columns specified by \code{parents}.
  }
  \item{parents}{
A character vector specifying the names of the parents that this particular child node has. When using this function as a node type in \code{\link{node}} or \code{\link{node_td}}, this argument usually does not need to be specified because the \code{formula} argument is required and contains all needed information already.
  }
  \item{formula}{
A \code{formula} object. The specific way this argument should be specified depends on the value of the \code{kind} argument used. It can be an expression (\code{kind="expr"}), a \code{simDAG} style enhanced formula to calculate the linear predictor only (\code{kind="linpred"}) or used as a way to store intermediary variable transformations (\code{kind="data"}).
  }
  \item{kind}{
A single character string specifying how the \code{formula} should be interpreted, with three allowed values: \code{"expr"}, \code{"linpred"} and \code{"data"}. If \code{"expr"} (default), the formula should contain a \code{~} symbol with nothing on the LHS, and any valid R expression that can be evaluated on \code{data} on the RHS. This expression needs to contain at least one variable name (otherwise users may simply use \code{\link{rconstant}} as node type). It may contain any number of function calls or other valid R syntax, given that all contained objects are included in the global environment. Note that the usual \code{formula} syntax, using for example \code{A:B*0.2} to specify an interaction won't work in this case. If that is the goal, users should use \code{kind="linpred"}, in which case the \code{formula} is interpreted in the normal \code{simDAG} way and the linear combination of the variables is calculated. Finally, if \code{kind="data"}, the \code{formula} may contain any enhanced \code{formula} syntax, such as \code{A:B} or \code{net()} calls, but it should not contain beta-coefficients or an \code{intercept}. In this case, the transformed variables are returned in the order given, using the \code{name} as column names.
  }
  \item{betas}{
Only used internally when \code{kind="linpred"}.
  }
  \item{intercept}{
Only used internally when \code{kind="linpred"}. If no intercept should be present, it should still be added to the formula using a simple 0, for example \code{~ 0 + A*0.2 + B*0.3}
  }
  \item{name}{
The name(s) of the resulting data when using \code{kind="data"}. Should have the same length as the \code{formula}. Names are given in the same order as the variables appear in \code{formula}.
  }
}
\details{
When using \code{kind="expr"}, custom functions and objects can be used without issues in the \code{formula}, but they need to be present in the global environment, otherwise the underlying \code{eval()} function call will fail. Using this function outside of \code{\link{node}} or \code{\link{node_td}} is essentially equal to using \code{with(data, eval(formula))} (without the \code{~} in the \code{formula}). If \code{kind!="expr"}, this function cannot be used outside of a defined \code{DAG}.
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
