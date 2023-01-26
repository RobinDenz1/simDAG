\name{sim_from_dag}
\alias{sim_from_dag}

\title{
Simulate Data from a Given DAG and Node Information
}
\description{
This function can be used to generate data from a given DAG. It additionally requires information on node distributions, beta coefficients and, depending on the node type, more parameters such as intercepts.
}
\usage{
sim_from_dag(n_sim, root_nodes, child_nodes, sort_dag=TRUE,
             check_inputs=TRUE)
}
\arguments{
  \item{n_sim}{
A single number specifying how many observations should be generated.
  }
  \item{root_nodes}{
Either a \code{data.frame} with \code{n_sim} rows containing the values for the root noded that should be used, or a \code{list} object containing one \code{list} for each root node. See details.
  }
  \item{child_nodes}{
A list containing one list for each child node in the DAG of interest. See details.
  }
  \item{sort_dag}{
Whether to topologically sort the DAG before starting the simulation or not. If the nodes in \code{child_nodes} are already topologically sorted, this argument can be set to \code{FALSE} to safe some computation time. This usually won't safe too much time though, because it internally uses the \code{topological_sort} function from the \pkg{Rfast} package, which is very fast.
  }
  \item{check_inputs}{
Whether to perform plausibility checks for the user input or not. Is set to \code{TRUE} by default, but can be set to \code{FALSE} in order to speed things up when using this function in a simulation study or something similar.
  }
}
\details{

\strong{\emph{How it Works}}:

First, \code{n_sim} i.i.d. samples from the root nodes are drawn. Children of these nodes are then generated one by one according to specified relationships and causal coefficients. For example, lets suppose there are two root nodes, \code{age} and \code{sex}. Those are generated from a normal distribution and a bernoulli distribution respectively. Afterward, the child node \code{height} is generated using both of these variables as parents according to a linear regression with defined coefficients, intercept and sigma (random error). This works because every DAG has at least one topological ordering, which is a linear ordering of vertices such that for every directed edge u v, vertex u comes before v in the ordering. By using \code{sort_dag=TRUE} it is ensured that the nodes in \code{child_nodes} are processed in such an ordering.

This procedure is simple in theory, but can get very complex when manually coded. This function offers a simplified workflow by only requiring the user to define the \code{root_nodes} and \code{child_nodes} with appropriate information (see below). A sample of size \code{n_sim} is then generated from the DAG specified by those two arguments.

\strong{\emph{Specifying the \code{root_nodes} Argument}}:

If no \code{data.frame} is supplied directly to this argument, it should be a list. The lists inside this list correspond to one root node each and should contain exactly three named parameters:

\itemize{
  \item{\code{name}: A single character string specifying the name of the root node.}
  \item{\code{dist}: The name of the function (as string) that should be called to generate the values (for example \code{rnorm}).}
  \item{\code{params}: A named list of arguments passed to the function specified by \code{dist}.}
}

\strong{\emph{Specifying the \code{child_nodes} Argument}}:

Each list inside of the \code{child_nodes} list needs to include at least the following named parameters:

\itemize{
  \item{\code{name}: A single character string specifying the name of the root node.}
  \item{\code{parents}: A character vector containing the names of all parents of that node}
  \item{\code{type}: A character string specifying which type of \code{node_function} should be used to generate data for this node.}
  \item{\code{betas}: The causal coefficients used to generate the data from this node. Usually (but not always) this should be a numeric vector with one entry for each parent node.}
  \item{\code{...}: Further named arguments passed to the respective \code{node_function}.}
}

Depending on the type of node used, more arguments might be required. More information about allowed and required arguments as well as information about the data generation mechanism can be found in the documentation of each \code{node_function}.

\strong{\emph{Implemented Node Types}}:

Currently, the following node types are available:

\itemize{
  \item{"\link[=node_gaussian]{gaussian}": A node based on linear regression.}
  \item{"\link[=node_binomial]{binomial}": A node based on logistic regression.}
  \item{"\link[=node_multinomial]{multinomial}": A node based on multinomial regression.}
  \item{"\link[=node_poisson]{poisson}": A node based on poisson regression.}
  \item{"\link[=node_cox]{cox}": A node based on cox-regression.}
}

It is however, very simple to write a new custom \code{node_function}. All that is required is, that it takes the inputs \code{data} (the sample as generated so far), \code{parents} (a character vector specifying the parents) and \code{betas} (the causal coefficients) and outputs either a numeric vector containing \code{n_sim} entries, or a \code{data.frame} with \code{n_sim} rows. More information about this can be found on the \code{node_custom} documentation page.
}
\author{
Robin Denz
}
\examples{
\dontrun{
# 2 root nodes, 1 child node
root_nodes <- list(list(dist="rnorm",
                        params=list(mean=50, sd=4),
                        name="age"),
                   list(dist="rbernoulli",
                        params=list(p=0.5),
                        name="sex"))
child_nodes <- list(list(parents=c("sex", "age"),
                         type="gaussian",
                         name="bmi",
                         betas=c(1.1, 0.4),
                         intercept=12,
                         error=2))
sim_dat <- sim_from_dag(n_sim=10000, root_nodes=root_nodes,
                        child_nodes=child_nodes)
}
}
