\name{node_custom}
\alias{node_custom}

\title{
Create Your Own Function to Simulate a Node
}
\description{
This page describes in detail how to extend this code to allow the usage of nodes that are not directly implemented here.
}
\usage{
node_custom(data, parents, betas, ...)
}
\arguments{
  \item{data}{
A \code{data.frame} containing all columns specified by \code{parents}.
  }
  \item{parents}{
A character vector specifying the names of the parents that this particular child node has.
  }
  \item{betas}{
Some obeject that includes the needed beta-coefficients for your node.
  }
  \item{...}{
Any amount of additional arguments.
  }
}
\details{
The number of available types of nodes is limited, but this package allows the user to easily implement their own node types by writing a single custom function. Only X things are required for this to work properly: (1) the function name should start with \code{node_}, (2) the function should contain the arguments \code{data}, \code{parents} and \code{betas} as described above and (3) it should return either a vector of length \code{n_sim} or a \code{data.frame} with any number of columns and \code{n_sim} rows.

The function may include any amount of additional arguments specified by the user. If you think that your custom node type might be useful to others, please add the code to this R-package. This can be done by putting it in a separate .r file into the "R" folder. In this case, a documentation page should also be created and saved under the "man" folder. Following the conventions used in this package would be appreciated. Adding automated unit-tests under the "tests" folder would also be very nice, to ensure the validity of your code.

Some simple examples for custom nodes are given below.
}
\examples{
\dontrun{
# create a custom node function, which is just a gaussian node that
# includes truncation
node_gaussian_trunc <- function(data, parents, betas, intercept, error,
                                left, right) {
  out <- node_gaussian(data=data, parents=parents, betas=betas,
                       intercept=intercept, error=error)
  out <- ifelse(out <= left, left,
                ifelse(out >= right, right, out))
  return(out)
}

# another custom node function, which simply returns a sum of the parents
node_parents_sum <- function(data, parents, betas=NULL) {
  out <- rowSums(data[, parents])
  return(out)
}

# an example of using these new node types in a simulation
root_nodes <- list(list(dist="rnorm",
                        params=list(mean=50, sd=4),
                        name="age"),
                   list(dist="rbernoulli",
                        params=list(p=0.5),
                        name="sex"))
child_nodes <- list(list(parents=c("sex", "age"),
                         type="gaussian_trunc",
                         name="custom_node_1",
                         betas=c(1.1, 0.4),
                         intercept=-2,
                         error=2,
                         left=-1,
                         right=10),
                    list(parents=c("sex", "age"),
                         type="parents_sum",
                         name="custom_node_2",
                         betas=NULL))
sim_dat <- sim_from_DAG(n_sim=10000, root_nodes=root_nodes,
                        child_nodes=child_nodes)
}
}
