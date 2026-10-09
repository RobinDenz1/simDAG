\name{set_lazy_dag}
\alias{set_lazy_dag}

\title{
Create a fully defined \code{DAG} object from a \code{lazy_DAG} object
}
\description{
Given a un-evaluated \code{lazy_DAG} object, created using the \code{\link{lazy_dag}} function and a named list or vector of parameters, this function returns a fully specified \code{DAG} object that can then be used to run actual simulations using \code{\link{sim_from_dag}}, \code{\link{sim_discrete_time}} or \code{\link{sim_discrete_event}}. See details and \code{?lazy_dag} for more information.
}
\usage{
set_lazy_dag(lazy_dag, params)
}
\arguments{
  \item{lazy_dag}{
A \code{lazy_DAG} object created using the \code{\link{lazy_dag}} function. See \code{?lazy_dag} for more information on how to specify a lazy DAG. See \code{?node} for more information on how to specify a regular \code{DAG}.
  }
  \item{params}{
A named list or a named vector, containing one entry per \code{param()} call used in the creation of the \code{lazy_dag} object. These entries should be scalar values that can be subsituted directly into the DAG definition.
  }
}
\details{
Given the previously un-evaluated \code{lazy_DAG} object, this function substitutes all \code{param()} pieces with the corresponding parameter defined in \code{params} and then finally evaluates the \code{DAG}. Note that the \code{lazy_DAG} object itself is not changed in place, so that the \code{set_lazy_dag()} function may be used multiple times on the same \code{lazy_DAG} to create multiple different data generation processes.
}
\value{
Returns a \code{DAG} object.
}
\author{
Robin Denz
}
\examples{
library(simDAG)

# define a lazy DAG with one unspecified parameter
ldag <- lazy_dag(
  empty_dag() +
    node("A", type="rnorm", mean=1, sd=param(SD))
)

# get a real DAG object with SD = 1 and another with SD = 2
dag1 <- set_lazy_dag(ldag, params=c(SD=1))
dag2 <- set_lazy_dag(ldag, params=c(SD=2))

# define some lazy DAG, leaving the intercept of "death" and one beta
# coefficient of "smoking" unspecified
ldag <- lazy_dag(
  empty_dag() +
    node("age", type="rnorm", mean=10, sd=2) +
    node("sex", parents="", type="rbernoulli", p=0.5) +
    node("smoking", type="binomial",
         formula= ~ -2 + age*param(Sbeta) + sex*0.2) +
    node("death", type="binomial",
         formula= ~ param(Dintercept) + age*1 + sex*2)
)

# get a real DAG object from this with set values
dag3 <- set_lazy_dag(ldag, params=c(Sbeta=2.1, Dintercept=-10))
}
