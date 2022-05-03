\name{node_cox}
\alias{node_cox}

\title{
Simulate a Node Using Cox-Regression
}
\description{
Data from the parents is used to generate the node using cox-regression using the method of Bender et al. (XXXX).
}
\usage{
node_cox(data, parents, betas, surv_dist, lambda, gamma,
         cens_dist, cens_args)
}
\arguments{
  \item{data}{
A \code{data.frame} containing all columns specified by \code{parents}.
  }
  \item{parents}{
A character vector specifying the names of the parents that this particular child node has.
  }
  \item{betas}{
A numeric vector with length equal to \code{parents}, specifying the causal beta coefficients used to generate the node.
  }
  \item{surv_dist}{
A single character specifying the distribution that should be used when generating the survival times. Can be either \code{"weibull"} or \code{"exponential"}.
  }
  \item{lambda}{
A single number used as parameter defined by \code{surv_dist}.
  }
  \item{gamma}{
A single number used as parameter defined by \code{surv_dist}.
  }
  \item{cens_dist}{
A single character naming the distribution function that should be used to generate the censoring times. For example, \code{"runif"} could be used to generate uniformly distributed censoring times. Set to \code{NULL} to get no censoring.
  }
  \item{cens_args}{
A list of named arguments which will be passed to the function specified by the \code{cens_dist} argument.
  }
}
\details{
More explanation
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
                         type="cox",
                         name="death",
                         betas=c(1.1, 0.4),
                         intercept=-2))
sim_dat <- sim_from_DAG(n_sim=10000, root_nodes=root_nodes,
                        child_nodes=child_nodes)
}
}
