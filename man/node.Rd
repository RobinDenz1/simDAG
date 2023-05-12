\name{node}
\alias{node}

\title{
Create a node object to grow a DAG step-by-step
}
\description{
This function should be used in conjunction with the \code{\link{empty_dag}} function to create \code{DAG} objects, which can then be used to simulate data using the \code{\link{sim_from_dag}} function.
}
\usage{
node(name, type, parents=NULL, ...)
}
\arguments{
  \item{name}{
A single character string specifying the name of the node.
  }
  \item{type}{
A single character string specifying the type of the node. Depending on whether the node is a root node or a child node, different node types are allowed. See details.
  }
  \item{parents}{
A character vector of names, specifying the parents of the node or \code{NULL} (default). If \code{NULL}, the node is treated as a root node. For convenience it is also allowed to set \code{parents=""} to indicate that the node is a root node.
  }
  \item{...}{
Further named arguments needed to specify the node. Those can be parameters of distribution functions such as the \code{p} argument in the \code{\link{rbernoulli}} function for root nodes or arbitrary named arguments such as the \code{betas} argument of the \code{\link{node_gaussian}} function.
  }
}
\details{

To generate data using the \code{\link{sim_from_dag}} function, it is required to create a \code{DAG} object first. This object needs to contain information about the causal structure of the data (e.g. which variable causes which variable) and the specific structural equations for each variable (information about causal coefficients, type of distribution etc.). In this package, the \code{node} function is used in conjunction with the \code{\link{empty_dag}} function to create this object.

This works by first initializing an empty \code{DAG} using the \code{\link{empty_dag}} function and then adding multiple calls to the \code{node} function to it using a simple \code{+}, where each call to \code{node} adds information about a single node that should be generated. Multiple examples are given below.

In each call to \code{node} the user needs to indicate what the node should be called (\code{name}), which function should be used to generate the node (\code{type}), whether the node has any parents and if so which (\code{parents}) and any additional arguments needed to actually call the data-generating function of this node later passed to the three-dot syntax (\code{...}).

\strong{\emph{Implemented Root Node Types}}:

Any function can be used to generate root nodes. The only requirement is that the function has at least one named argument called \code{n} which controls the length of the resulting vector. For example, the user could specify a node of type \code{"rnorm"} to create a normally distributed node with no parents. The argument \code{n} will be set internally, but any additional arguments can be specified using the \code{...} syntax. In the \code{type="rnorm"} example, the user could set the mean and standard deviation using \code{node(name="example", type="rnorm", mean=10, sd=5)}.

For convenience, this package includes three custom root-node functions:

\itemize{
  \item{"\link[=rbernoulli]{rbernoulli}": Draws randomly from a bernoulli distribution.}
  \item{"\link[=rcategorical]{rcategorical}": Draws randomly from any discrete probability density function.}
  \item{"\link[=rconstant]{rconstant}": Used to set a variable to a constant value.}
}

\strong{\emph{Implemented Node Types}}:

Currently, the following node types are implemented directly for convenience:

\itemize{
  \item{"\link[=node_gaussian]{gaussian}": A node based on linear regression.}
  \item{"\link[=node_binomial]{binomial}": A node based on logistic regression.}
  \item{"\link[=node_conditional_prob]{conditional_prob}": A node based on conditional probabilities.}
  \item{"\link[=node_conditional_distr]{conditional_distr}": A node based on conditional draws from different distributions.}
  \item{"\link[=node_multinomial]{multinomial}": A node based on multinomial regression.}
  \item{"\link[=node_poisson]{poisson}": A node based on poisson regression.}
  \item{"\link[=node_negative_binomial]{negative_binomial}": A node based on negative binomial regression.}
  \item{"\link[=node_cox]{cox}": A node based on cox-regression.}
}

However, it is very simple to write a new custom \code{node_function} to be used instead, allowing the user to use any \code{type} of data-generation mechanism for any node. All that is required of this function is, that it has the named arguments \code{data} (the sample as generated so far) and \code{parents} (a character vector specifying the parents) and outputs either a vector containing \code{n_sim} entries, or a \code{data.frame} with \code{n_sim} rows and an arbitrary amount of columns. More information about this can be found on the \code{node_custom} documentation page.

}
\value{
Returns a \code{DAG.node} object which can be added to a \code{DAG} object directly.
}
\author{
Robin Denz
}
\examples{
## add some examples
}
