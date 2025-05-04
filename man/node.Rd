\name{node}
\alias{node}
\alias{node_td}

\title{
Create a node object to grow a DAG step-by-step
}
\description{
These functions should be used in conjunction with the \code{\link{empty_dag}} function to create \code{DAG} objects, which can then be used to simulate data using the \code{\link{sim_from_dag}} function or the \code{\link{sim_discrete_time}} function.
}
\usage{
node(name, type, parents=NULL, formula=NULL, ...)

node_td(name, type, parents=NULL, formula=NULL, ...)
}
\arguments{
  \item{name}{
A character vector with at least one entry specifying the name of the node. If a character vector containing multiple different names is supplied, one separate node will be created for each name. These nodes are completely independent, but have the exact same node definition as supplied by the user. If only a single character string is provided, only one node is generated.
  }
  \item{type}{
A single character string specifying the type of the node. Depending on whether the node is a root node, a child node or a time-dependent node different node types are allowed. See details. Alternatively, a suitable function may be passed directly to this argument.
  }
  \item{parents}{
A character vector of names, specifying the parents of the node or \code{NULL} (default). If \code{NULL}, the node is treated as a root node. For convenience it is also allowed to set \code{parents=""} to indicate that the node is a root node.
  }
  \item{formula}{
An optional \code{formula} object to describe how the node should be generated or \code{NULL} (default). If supplied it should start with \code{~}, having nothing else on the left hand side. The right hand side should define the entire structural equation, including the betas and intercepts. It may contain any valid formula syntax, such as \code{~ -2 + A*3 + B*4} or \code{~ -2 + A*3 + B*4 + I(A^2)*0.3 + A:B*1.1}, allowing arbitrary non-linear effects, arbitrary interactions and multiple coefficients for categorical variables. Additionally, for some node types, random effects and random slopes are supported. If this argument is defined, there is no need to define the \code{betas} and \code{intercept} argument. The \code{parents} argument should still be specified whenever a categorical variable is used in the formula. This argument is supported for build-in nodes of type \code{"binomial"}, \code{"gaussian"}, \code{"poisson"}, \code{"negative_binomial"}, \code{"cox"}, \code{"aftreg"}, \code{"ahreg"}, \code{"ehreg"}, \code{"poreg"} and \code{"ypreg"} and for any custom node defined by the user. It is also supported for nodes of type \code{"identity"}, but slightly different input is expected in that case. See examples and the associated vignette for an in-depth explanation.
  }
  \item{...}{
Further named arguments needed to specify the node. Those can be parameters of distribution functions such as the \code{p} argument in the \code{\link{rbernoulli}} function for root nodes or arbitrary named arguments such as the \code{betas} argument of the \code{\link{node_gaussian}} function.
  }
}
\details{

To generate data using the \code{\link{sim_from_dag}} function or the \code{\link{sim_discrete_time}} function, it is required to create a \code{DAG} object first. This object needs to contain information about the causal structure of the data (e.g. which variable causes which variable) and the specific structural equations for each variable (information about causal coefficients, type of distribution etc.). In this package, the \code{node} and/or \code{node_td} function is used in conjunction with the \code{\link{empty_dag}} function to create this object.

This works by first initializing an empty \code{DAG} using the \code{\link{empty_dag}} function and then adding multiple calls to the \code{node} and/or \code{node_td} functions to it using a simple \code{+}, where each call to \code{node} and/or \code{node_td} adds information about a single node that should be generated. Multiple examples are given below.

In each call to \code{node} or \code{node_td} the user needs to indicate what the node should be called (\code{name}), which function should be used to generate the node (\code{type}), whether the node has any parents and if so which (\code{parents}) and any additional arguments needed to actually call the data-generating function of this node later passed to the three-dot syntax (\code{...}).

\strong{\emph{\code{node} vs. \code{node_td}}}:

By calling \code{node} you are indicating that this node is a time-fixed variable which should only be generated once. By using \code{node_td} you are indicating that it is a time-dependent node, which will be updated at each step in time when using a discrete-time simulation.

\code{node_td} should only be used if you are planning to perform a discrete-time simulation with the \code{\link{sim_discrete_time}} function. \code{DAG} objects including time-dependent nodes may not be used in the \code{\link{sim_from_dag}} function.

\strong{\emph{Implemented Root Node Types}}:

Any function can be used to generate root nodes. The only requirement is that the function has at least one named argument called \code{n} which controls the length of the resulting vector. For example, the user could specify a node of type \code{"rnorm"} to create a normally distributed node with no parents. The argument \code{n} will be set internally, but any additional arguments can be specified using the \code{...} syntax. In the \code{type="rnorm"} example, the user could set the mean and standard deviation using \code{node(name="example", type="rnorm", mean=10, sd=5)}.

For convenience, this package additionally includes three custom root-node functions:

\itemize{
  \item{"\link[=rbernoulli]{rbernoulli}": Draws randomly from a bernoulli distribution.}
  \item{"\link[=rcategorical]{rcategorical}": Draws randomly from any discrete probability density function.}
  \item{"\link[=rconstant]{rconstant}": Used to set a variable to a constant value.}
}

\strong{\emph{Implemented Child Node Types}}:

Currently, the following node types are implemented directly for convenience:

\itemize{
  \item{"\link[=node_gaussian]{gaussian}": A node based on (mixed) linear regression.}
  \item{"\link[=node_binomial]{binomial}": A node based on (mixed) logistic regression.}
  \item{"\link[=node_conditional_prob]{conditional_prob}": A node based on conditional probabilities.}
  \item{"\link[=node_conditional_distr]{conditional_distr}": A node based on conditional draws from different distributions.}
  \item{"\link[=node_multinomial]{multinomial}": A node based on multinomial regression.}
  \item{"\link[=node_poisson]{poisson}": A node based on (mixed) poisson regression.}
  \item{"\link[=node_negative_binomial]{negative_binomial}": A node based on negative binomial regression.}
  \item{"\link[=node_zeroinfl]{zeroinfl}": A node based on a zero-inflated poisson or negative binomial regression.}
  \item{"\link[=node_identity]{identity}": A node that is just some R expression of other nodes.}
  \item{"\link[=node_mixture]{mixture}": A node that is a mixture of different node definitions.}
  \item{"\link[=node_cox]{cox}": A node based on cox-regression.}
  \item{"\link[=node_aftreg]{aftreg}": A node based on an accelerated failure time model.}
  \item{"\link[=node_ahreg]{ahreg}": A node based on an accelerated hazard model.}
  \item{"\link[=node_ehreg]{ehreg}": A node based on a extended hazard model.}
  \item{"\link[=node_poreg]{poreg}": A node based on a proportional odds model.}
  \item{"\link[=node_ypreg]{ypreg}": A node based on a Young and Prentice model.}
}

For custom child node types, see below or consult the vignette on custom node definitions.

\strong{\emph{Implemented Time-Dependent Node Types}}:

Currently, the following node types are implemented directly for convenience to use in \code{node_td} calls:

\itemize{
  \item{"\link[=node_time_to_event]{time_to_event}": A node based on repeatedly checking whether an event occurs at each point in time.}
  \item{"\link[=node_competing_events]{competing_events}": A node based on repeatedly checking whether one of multiple mutually exclusive events occurs at each point in time.}
}

However, the user may also use any of the child node types in a \code{node_td} call directly. For custom time-dependent node types, please consult the associated vignette.

\strong{\emph{Custom Node Types}}

It is very simple to write a new custom \code{node_function} to be used instead, allowing the user to use any \code{type} of data-generation mechanism for any type of node (root / child / time-dependent). All that is required of this function is, that it has the named arguments \code{data} (the sample as generated so far) and, if it's a child node, \code{parents} (a character vector specifying the parents) and outputs either a vector containing \code{n_sim} entries, or a \code{data.frame} with \code{n_sim} rows and an arbitrary amount of columns. More information about this can be found in the associated vignette: \code{vignette(topic="v_custom_nodes", package="simDAG")}.

\strong{\emph{Using child nodes as parents for other nodes}}:

If the data generated by a child node is categorical (such as when using \code{node_multinomial}) they can still be used as parents of other nodes for most standard node types without issues. All the user has to do is to use \code{formula} argument to supply an enhanced formula, instead of defining the \code{parents} and \code{betas} argument directly. This works well for all node types that directly support \code{formula} input and for all custom nodes specified by the user. See the associated vignette: \code{vignette(topic="v_using_formulas", package="simDAG")} for more information on how to correctly use formulas.

\strong{\emph{Cyclic causal structures}}:

The name DAG (directed \strong{acyclic} graph) implies that cycles are not allowed. This means that if you start from any node and only follow the arrows in the direction they are pointing, there should be no way to get back to your original node. This is necessary both theoretically and for practical reasons if we are dealing with static DAGs created using the \code{node} function. If the user attempts to generate data from a static cyclic graph using the \code{\link{sim_from_dag}} function, an error will be produced.

However, in the realm of discrete-time simulations, cyclic causal structures are perfectly reasonable. A variable \eqn{A} at \eqn{t = 1} may influence a variable \eqn{B} at \eqn{t = 2}, which in turn may influence variable \eqn{A} at \eqn{t = 3} again. Therefore, when using the \code{node_td} function to simulate time-dependent data using the \code{\link{sim_discrete_time}} function, cyclic structures are allowed to be present and no error will be produced.

}
\note{
Contrary to the R standard, this function does \bold{NOT} support partial matching of argument names. This means that supplying \code{nam="age"} will not be recognized as \code{name="age"} and instead will be added as additional node argument used in the respective data-generating function call when using \code{sim_from_dag}.
}
\value{
Returns a \code{DAG.node} object which can be added to a \code{DAG} object directly.
}
\author{
Robin Denz
}
\examples{
library(simDAG)

# creating a DAG with a single root node
dag <- empty_dag() +
  node("age", type="rnorm", mean=30, sd=4)

# creating a DAG with multiple root nodes
# (passing the functions directly to 'type' works too)
dag <- empty_dag() +
  node("sex", type=rbernoulli, p=0.5) +
  node("income", type=rnorm, mean=2700, sd=500)

# creating a DAG with multiple root nodes + multiple names in one node
dag <- empty_dag() +
  node("sex", type="rbernoulli", p=0.5) +
  node(c("income_1", "income_2"), type="rnorm", mean=2700, sd=500)

# also using child nodes
dag <- empty_dag() +
  node("sex", type="rbernoulli", p=0.5) +
  node("income", type="rnorm", mean=2700, sd=500) +
  node("sickness", type="binomial", parents=c("sex", "income"),
       betas=c(1.2, -0.3), intercept=-15) +
  node("death", type="binomial", parents=c("sex", "income", "sickness"),
       betas=c(0.1, -0.4, 0.8), intercept=-20)

# creating the same DAG as above, but using the enhanced formula interface
dag <- empty_dag() +
  node("sex", type="rbernoulli", p=0.5) +
  node("income", type="rnorm", mean=2700, sd=500) +
  node("sickness", type="binomial",
       formula= ~ -15 + sexTRUE*1.2 + income*-0.3) +
  node("death", type="binomial",
       formula= ~ -20 + sexTRUE*0.1 + income*-0.4 + sickness*0.8)

# using time-dependent nodes
# NOTE: to simulate data from this DAG, the sim_discrete_time() function needs
#       to be used due to "sickness" being a time-dependent node
dag <- empty_dag() +
  node("sex", type="rbernoulli", p=0.5) +
  node("income", type="rnorm", mean=2700, sd=500) +
  node_td("sickness", type="binomial", parents=c("sex", "income"),
          betas=c(0.1, -0.4), intercept=-50)

# we could also use a DAG with only time-varying variables
dag <- empty_dag() +
  node_td("vaccine", type="time_to_event", prob_fun=0.001, event_duration=21) +
  node_td("covid", type="time_to_event", prob_fun=0.01, event_duration=15,
          immunity_duration=100)
}
