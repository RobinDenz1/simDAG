\name{do}
\alias{do}

\title{
Pearls do-operator for \code{DAG} objects
}
\description{
This function can be used to set one or more nodes in a given \code{DAG} object to a specific value, which corresponds to an intervention on a DAG as defined by the do-operator introduced by Judea Pearl.
}
\usage{
do(dag, names, values)
}
\arguments{
  \item{dag}{
A \code{DAG} object created using the \code{\link{empty_dag}} and \code{\link{node}} functions. See \code{?node} for more information on how to specify a DAG.
  }
  \item{names}{
A character string specifying names of nodes in the \code{dag} object. The value of these nodes will be set to the corresponding value specified in the \code{values} argument. If the node is not already defined in \code{dag}, a new one will be added without warning.
  }
  \item{values}{
A vector or list of any values or \code{\link{node}} / \code{\link{node_td}} definitions. These nodes defined with the \code{names} argument will be set to those values or the new node definitions.
  }
}
\details{

Internally this function simply removes the old node definition of all nodes in \code{names} and replaces it with a new node definition that defines the node as a constant value, irrespective of the original definition, if the corresponding entry in \code{values} is a signle value. If it is a new \code{\link{node}} or \code{\link{node_td}} instead, the new definition replaces the old one. The same effect can be created by directly specifying the \code{DAG} in this way from the start (see examples).

This function does not alter the original \code{DAG} in place. Instead, it returns a modified version of the \code{DAG}. In other words, using only \code{do(dag, names="A", values=3)} will not change the \code{dag} object.

}
\value{
Returns a \code{DAG} object with updated node definitions.
}
\author{
Robin Denz
}
\references{
Judea Pearl (2009). Causality: Models, Reasoning and Inference. 2nd ed. Cambridge: Cambridge University Press
}
\examples{
library(simDAG)

# define some initial DAG
dag <- empty_dag() +
  node("death", "binomial", c("age", "sex"), betas=c(1, 2), intercept=-10) +
  node("age", type="rnorm", mean=10, sd=2) +
  node("sex", parents="", type="rbernoulli", p=0.5) +
  node("smoking", parents=c("sex", "age"), type="binomial",
       betas=c(0.6, 0.2), intercept=-2)

# return new DAG with do(smoking = TRUE)
dag2 <- do(dag, names="smoking", values=TRUE)

# which is equivalent to
dag2 <- empty_dag() +
  node("death", "binomial", c("age", "sex"), betas=c(1, 2), intercept=-10) +
  node("age", type="rnorm", mean=10, sd=2) +
  node("sex", parents="", type="rbernoulli", p=0.5) +
  node("smoking", type="rconstant", constant=TRUE)

# use do() on multiple variables: do(smoking = TRUE, sex = FALSE)
dag2 <- do(dag, names=c("smoking", "sex"), values=list(TRUE, FALSE))

## set node in DAG to a completely new definition
dag <- empty_dag() +
  node("death", "binomial", c("age", "sex"), betas=c(1, 2), intercept=-10) +
  node("age", type="rnorm", mean=10, sd=2) +
  node("sex", parents="", type="rbernoulli", p=0.5) +
  node("smoking", parents=c("sex", "age"), type="binomial",
       betas=c(0.6, 0.2), intercept=-2)

dag2 <- do(dag, names="smoking", values=list(
  node(".", type="poisson", formula= ~ -2 + age*0.2 + sex*1.2)
))
}
