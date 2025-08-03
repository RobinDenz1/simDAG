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
              betas, intercept, var_names=NULL,
              name=NULL, dag=NULL)
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
A single character string specifying how the \code{formula} should be interpreted, with three allowed values: \code{"expr"}, \code{"linpred"} and \code{"data"}. If \code{"expr"} (default), the formula should contain a \code{~} symbol with nothing on the LHS, and any valid R expression that can be evaluated on \code{data} on the RHS. This expression needs to contain at least one variable name (otherwise users may simply use \code{\link{rconstant}} as node type). It may contain any number of function calls or other valid R syntax, given that all contained objects are included in the global environment. Note that the usual \code{formula} syntax, using for example \code{A:B*0.2} to specify an interaction won't work in this case. If that is the goal, users should use \code{kind="linpred"}, in which case the \code{formula} is interpreted in the normal \code{simDAG} way and the linear combination of the variables is calculated. Finally, if \code{kind="data"}, the \code{formula} may contain any enhanced \code{formula} syntax, such as \code{A:B} or \code{net()} calls, but it should not contain beta-coefficients or an \code{intercept}. In this case, the transformed variables are returned in the order given, using the \code{name} as column names. See examples.
  }
  \item{betas}{
Only used internally when \code{kind="linpred"}.
  }
  \item{intercept}{
Only used internally when \code{kind="linpred"}. If no intercept should be present, it should still be added to the formula using a simple 0, for example \code{~ 0 + A*0.2 + B*0.3}
  }
  \item{var_names}{
Only used when \code{kind="data"}. In this case, and only if there are multiple terms on the right-hand side of \code{formula}, the resulting columns will be re-named accoring to this argument. Should have the same length as the number of terms in \code{formula}. Names are given in the same order as the variables appear in \code{formula}. If only a single term is on the right-hand side of \code{formula}, the \code{name} supplied in the \code{\link{node}} function call will automatically be used as the nodes name and this argument is ignored. Set to \code{NULL} (default) to just use the terms as names.
  }
  \item{name}{
A single character string, specifying the name of the node. Passed internally only. See \code{var_names}.
  }
  \item{dag}{
The \code{dag} that this node is a part of. Will be passed internally if needed (for example when performing networks-based simulations). This argument can therefore always be ignored by users.
  }
}
\details{
When using \code{kind="expr"}, custom functions and objects can be used without issues in the \code{formula}, but they need to be present in the global environment, otherwise the underlying \code{eval()} function call will fail. Using this function outside of \code{\link{node}} or \code{\link{node_td}} is essentially equal to using \code{with(data, eval(formula))} (without the \code{~} in the \code{formula}). If \code{kind!="expr"}, this function cannot be used outside of a defined \code{DAG}.

Please note that when using identity nodes with \code{kind="data"} and multiple terms in \code{formula}, the printed structural equations and plots of a \code{dag} object may not be correct.
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

#### using kind = "expr" ####

# define a DAG
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("something", type="identity", formula= ~ age + sex + 2)

sim_dat <- sim_from_dag(dag=dag, n_sim=100)
head(sim_dat)

# more complex alternative
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("something", type="identity",
       formula= ~ age / 2 + age^2 - ifelse(sex, 2, 3) + 2)

sim_dat <- sim_from_dag(dag=dag, n_sim=100)
head(sim_dat)

#### using kind = "linpred" ####

# this would work with both kind="expr" and kind="linpred"
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("pred", type="identity", formula= ~ 1 + age*0.2 + sex*1.2,
       kind="linpred")

sim_dat <- sim_from_dag(dag=dag, n_sim=10)
head(sim_dat)

# this only works with kind="linpred", due to the presence of a special term
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5, output="numeric") +
  node("pred", type="identity", formula= ~ 1 + age*0.2 + sex*1.2 + age:sex*-2,
       kind="linpred")

sim_dat <- sim_from_dag(dag=dag, n_sim=10)
head(sim_dat)

#### using kind = "data" ####

# simply return the transformed data, useful if the terms are used
# frequently in multiple nodes in the DAG to save computation time

# using only a single interaction term
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5, output="numeric") +
  node("age_sex_interact", type="identity", formula= ~ age:sex, kind="data")

sim_dat <- sim_from_dag(dag=dag, n_sim=10)
head(sim_dat)

# using multiple terms
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5, output="numeric") +
  node("name_not_used", type="identity", formula= ~ age:sex + I(age^2),
       kind="data", var_names=c("age_sex_interact", "age_squared"))

sim_dat <- sim_from_dag(dag=dag, n_sim=10)
head(sim_dat)
}
