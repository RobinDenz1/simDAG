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
\author{
Robin Denz
}
\examples{
library(simDAG)

set.seed(5425)

# define needed DAG
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("smoking", type="binomial", parents=c("age", "sex"),
       betas=c(1.1, 0.4), intercept=-2)

# simulate data from it
sim_dat <- sim_from_dag(dag=dag, n_sim=100)

# returning only the estimated probability instead
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("smoking", type="binomial", parents=c("age", "sex"),
       betas=c(1.1, 0.4), intercept=-2, return_prob=TRUE)

sim_dat <- sim_from_dag(dag=dag, n_sim=100)
}
