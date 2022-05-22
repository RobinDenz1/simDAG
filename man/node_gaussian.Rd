\name{node_gaussian}
\alias{node_gaussian}

\title{
Simulate a Node Using Linear Regression
}
\description{
Data from the parents is used to generate the node using linear regression by predicting the covariate specific mean and sampling from a normal distribution with that mean and a specified standard deviation.
}
\usage{
node_gaussian(data, parents, betas, intercept, error)
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
  \item{intercept}{
A single number specifying the intercept that should be used when generating the node.
  }
  \item{error}{
A single number specifying the sigma error that should be used when generating the node.
  }
}
\details{
Using the general linear regression equation, the observation-specific value that would be expected given the model is generated for every observation in the dataset generarted thus far. We could stop here, but this would create a perfect fit for the node, which is unrealistic. Instead, we add an error term by taking one sample of a normal distribution for each observation with mean zero and standard deviation \code{error}. This error term is then added to the predicted mean.
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
sim_dat <- sim_from_DAG(n_sim=10000, root_nodes=root_nodes,
                        child_nodes=child_nodes)
}
}
