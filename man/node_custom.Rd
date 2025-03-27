\name{node_custom}
\alias{node_custom}

\title{
Create Your Own Function to Simulate a Root Node, Child Node or Time-Dependent Node
}
\description{
This page describes in detail how to define custom functions to allow the usage of root nodes, child nodes or time-dependent nodes that are not directly implemented in this package. By doing so, users may create data with any functional dependence they can think of.
}
\details{
The number of available types of nodes is limited, but this package allows the user to easily implement their own node types by writing a single custom function. Users may create their own root nodes, child nodes and time-dependent nodes. The requirements for each node type are listed below. Some simple examples for each node type are given further below.

If you think that your custom node type might be useful to others, please contact the maintainer of this package via the supplied e-mail address or github and we might add it to this package.

\strong{\emph{Root Nodes}}:

Any function that generates some vector of size \eqn{n} with \code{n==nrow(data)}, or a \code{data.frame} with as many rows as the current data can be used as a child node. The only requirement is:

\itemize{
  \item \strong{1.)} The function should have an argument called \code{n} which controls how many samples to generate.
}

Some examples that are already implemented in R outside of this package are \code{rnorm()}, \code{rgamma()} and \code{rbeta()}. The function may take any amount of further arguments, which will be passed through the three-dot syntax. Note that whenever the supplied function produces a \code{data.frame} (or similar object), the user has to ensure that the included columns are named properly.

\strong{\emph{Child Nodes}}:

Again, almost any function may be used to generate a child node. Only four things are required for this to work properly:

\itemize{
  \item \strong{1.)} Its' name should start with \code{node_} (if you want to use a string to define it in \code{type}).
  \item \strong{2.)} It should contain an argument called \code{data} (contains the already generated data).
  \item \strong{3.)} It should contain an argument called \code{parents} (contains a vector of the child nodes parents).
  \item \strong{4.)} It should return either a vector of length \code{n_sim} or a \code{data.frame} with any number of columns and \code{n_sim} rows.
}

The function may include any amount of additional arguments specified by the user.

\strong{\emph{Time-Dependent Nodes}}:

By time-dependent nodes we mean nodes that are created using the \code{\link{node_td}} function. In general, this works in essentially the same way as for simple root nodes or child nodes. The requirements are:

\itemize{
  \item \strong{1.)} Its' name should start with \code{node_} (if you want to use a string to define it in \code{type}).
  \item \strong{2.)} It should contain an argument called \code{data} (contains the already generated data).
  \item \strong{3.)} If it is a child node, it should contain an argument called \code{parents} (contains a vector of the child nodes parents). This is not necessary for nodes that are independently generated.
  \item \strong{4.)} It should return either a vector of length \code{n_sim} or a \code{data.frame} with any number of columns and \code{n_sim} rows.
}

Again, any number of additional arguments is allowed and will be passed through the three-dot syntax. Additionally, users may add an argument to this function called \code{sim_time}. If included in the function definition, the current time of the simulation will be passed to the function on every call made to it. Similarly, the argument \code{past_states} may be added. If done so, a list containing all previous states of the simulation (as saved using the \code{save_states} argument of the \code{\link{sim_discrete_time}}) function) will be passed to it internally, giving the user access to the data generated at previous points in time.

Users may also use the enhanced \code{formula} interface directly with custom child nodes and custom time-dependent nodes. This is described in detail in the vignette on specifying formulas (see \code{vignette(topic="v_using_formulas", package="simDAG")}).
}
\value{
Should return either a vector of length \code{nrow(data)} or a \code{data.table} or \code{data.frame} with \code{nrow(data)} rows.
}
\author{
Robin Denz
}
\examples{
library(simDAG)

set.seed(3545)

################ Custom Root Nodes ###################

# using external functions without defining them yourself can be done this way
dag <- empty_dag() +
  node("A", type="rgamma", shape=0.1, rate=2) +
  node("B", type="rbeta", shape1=2, shape2=0.3)

## define your own root node instead
# this function takes the sum of a normally distributed random number and an
# uniformly distributed random number
custom_root <- function(n, min=0, max=1, mean=0, sd=1) {
  out <- runif(n, min=min, max=max) + rnorm(n, mean=mean, sd=sd)
  return(out)
}

dag <- empty_dag() +
  node("A", type="custom_root", min=0, max=10, mean=5, sd=2)

# equivalently, the function can be supplied directly
dag <- empty_dag() +
  node("A", type=custom_root, min=0, max=10, mean=5, sd=2)

############### Custom Child Nodes ###################

# create a custom node function, which is just a gaussian node that
# includes (bad) truncation
node_gaussian_trunc <- function(data, parents, betas, intercept, error,
                                left, right) {
  out <- node_gaussian(data=data, parents=parents, betas=betas,
                       intercept=intercept, error=error)
  out <- ifelse(out <= left, left,
                ifelse(out >= right, right, out))
  return(out)
}

# another custom node function, which simply returns a sum of the parents
parents_sum <- function(data, parents, betas=NULL) {
  out <- rowSums(data[, parents, with=FALSE])
  return(out)
}

# an example of using these new node types in a simulation
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("custom_1", type="gaussian_trunc", parents=c("sex", "age"),
       betas=c(1.1, 0.4), intercept=-2, error=2, left=10, right=25) +
  node("custom_2", type=parents_sum, parents=c("age", "custom_1"))

sim_dat <- sim_from_dag(dag=dag, n_sim=100)

########## Custom Time-Dependent Nodes ###############

## example for a custom time-dependent node with no parents
# this node simply draws a new value from a normal distribution at
# each point in time
node_custom_root_td <- function(data, n, mean=0, sd=1) {
  return(rnorm(n=n, mean=mean, sd=sd))
}

n_sim <- 100

dag <- empty_dag() +
  node_td(name="Something", type=node_custom_root_td, n=n_sim, mean=10, sd=5)

sim <- sim_discrete_time(dag, n_sim=n_sim, max_t=10)

## example for a custom time-dependent child node
# draw from a normal distribution with different specifications based on
# whether a previously updated time-dependent node is currently TRUE
node_custom_child <- function(data, parents) {
  out <- numeric(nrow(data))
  out[data$other_event] <- rnorm(n=sum(data$other_event), mean=10, sd=3)
  out[!data$other_event] <- rnorm(n=sum(!data$other_event), mean=5, sd=10)
  return(out)
}

dag <- empty_dag() +
  node_td("other", type="time_to_event", prob_fun=0.1) +
  node_td("whatever", type="custom_child", parents="other_event")

sim <- sim_discrete_time(dag, n_sim=50, max_t=10)

## using the sim_time argument in a custom node function
# this function returns a continuous variable that is simply the
# current simulation time squared
node_square_sim_time <- function(data, sim_time, n_sim) {
  return(rep(sim_time^2, n=n_sim))
}

# note that we should not actually define the sim_time argument in the
# node_td() call below, because it will be passed internally, just like data
dag <- empty_dag() +
  node_td("unclear", type=node_square_sim_time, n_sim=100)

sim <- sim_discrete_time(dag, n_sim=100, max_t=10)

## a node using previous states of the simulation

# this function simply returns the value used two simulation time steps ago +
# a normally distributed random value
node_prev_state <- function(data, past_states, sim_time) {
  if (sim_time < 3) {
    return(rnorm(n=nrow(data)))
  } else {
    return(past_states[[sim_time-2]]$A + rnorm(n=nrow(data)))
  }
}

# note that we again do not specify the sim_time and past_states argument
# directly here, because they are set internally
dag <- empty_dag() +
  node_td("A", type=node_prev_state, parents="A")

# save_states="all" is needed, because we use them internally
sim <- sim_discrete_time(dag, n_sim=100, max_t=10, save_states="all")
}
