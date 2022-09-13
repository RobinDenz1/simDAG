\name{node_binomial}
\alias{node_binomial}

\title{
Simulate a Node Using Logistic Regression
}
\description{
Data from the parents is used to generate the node using logistic regression by predicting the covariate specific probability of 1 and sampling from a bernoulli distribution accordingly.
}
\usage{
node_binomial(data, parents, betas, intercept,
              return_prob=FALSE)
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
  \item{return_prob}{
Either \code{TRUE} or \code{FALSE} (default). If \code{TRUE}, the calculated probability is returned instead of the results of bernoulli trials.
  }
}
\details{
Using the normal form a logistic regression model, the observation specific event probability is generated for every observation in the dataset. Using the \code{rbernoulli} function, this probability is then used to take one bernoulli sample for each observation in the dataset. If only the probability should be returned \code{return_prob} should be set to \code{TRUE}.
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
                         type="binomial",
                         name="smoking",
                         betas=c(1.1, 0.4),
                         intercept=-2))
sim_dat <- sim_from_dag(n_sim=10000, root_nodes=root_nodes,
                        child_nodes=child_nodes)
}
}
