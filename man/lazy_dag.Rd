\name{lazy_dag}
\alias{lazy_dag}

\title{
Define a partial \code{DAG} object without evaluating it
}
\description{
This function should be used as a wrapper around an expression that aims to generate a \code{DAG} object, but with some parameters left un-specified. Those parameters should be defined using the \code{param()} syntax, as shown below. The \code{DAG} is then not instantly evaluated. Instead, it will only be evaluated later using a \code{\link{set_lazy_dag}} call, which then sets the still un-specified parameters. This may be useful to create flexible functions that generate \code{DAG} objects during simulations, and is also used in \code{calibrate_dag}.
}
\usage{
lazy_dag(expr)
}
\arguments{
  \item{expr}{
An expression that would usually create a \code{DAG} object. This should include one \code{\link{empty_dag}} call and one or more added \code{\link{node}} or \code{\link{node_td}} functions. Importantly, at least one parameter of one node should be left unspecified, using the \code{param()} syntax. See \code{?node} for more information on how to specify a regular DAG. See details on how to use the \code{param()} syntax.
  }
}
\details{
It is sometimes convenient to define a \code{DAG} object, but to leave some parameters of it flexible. For example, if we wanted to conduct a Monte-Carlo simulation study with different values of an interaction coefficient in a \code{formula} of a \code{\link{node}}, we need different \code{DAG} definitions for each scenario. Instead of copying and pasting nearly the same code multiple times, this function allows users to define a single partially specified \code{DAG} that leaves one or multiple parameters of a \code{DAG} definition unspecified.

This works the same way a usual \code{DAG} construction does. The only two differences are (1) that the whole expression (\code{expr}) that creates the \code{DAG} should be wrapped in a \code{lazy_dag()} function call and (2) that parameters that should remain flexible are wrapped in a \code{param()} function. For example, if we wish to leave the intercept in a \code{formula} flexible, we could use \code{param(intercept)}. There is no limit on the amount of flexible parameters that can be specified. They also do not have to be in a \code{formula}, but can be passed to any argument of any \code{node} call. The only requirements are that the names used inside the \code{param()} call are unique, so that they can later be unambiguously identified.

Once a \code{lazy_DAG} is created, users may create real \code{DAG} objects from it using the \code{\link{set_lazy_dag}} function, which only requires the \code{lazy_DAG} and a vector of \code{params} as input. See \code{?set_lazy_dag} for more information. Before running \code{set_lazy_dag()}, it is not possible to use these objects to simulate any data, because they are not fully specified or even executed yet. Note also that \code{param()} is just a piece of syntax. The \code{param()} function itself does not actually exist in the \pkg{simDAG} package.
}
\value{
Returns a \code{lazy_DAG} object.
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
