\name{node_multinomial}
\alias{node_multinomial}

\title{
Simulate a Node Using Multinomial Regression
}
\description{
Data from the parents is used to generate the node using multinomial regression by predicting the covariate specific probability of each class and sampling from a multinomial distribution accordingly.
}
\usage{
node_multinomial(data, parents, betas, intercepts,
                 labels=NULL, coerce2factor=TRUE,
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
A numeric matrix with \code{length(parents)} columns and one row for each class that should be simulated, specifying the causal beta coefficients used to generate the node.
  }
  \item{intercepts}{
A numeric vector with one entry for each class that should be simulated, specifying the intercepts used to generate the node.
  }
  \item{labels}{
An optional character vector giving the factor levels of the generated classes. If \code{NULL} (default), the integers are simply used as factor levels.
  }
  \item{coerce2factor}{
A single logical value specifying whether to return the drawn events as a factor (default) or as integers.
  }
  \item{return_prob}{
Either \code{TRUE} or \code{FALSE} (default). Specifies whether to return the matrix of class probabilities or not. If you are using this function inside of a \code{\link{node}} call, you cannot set this to \code{TRUE} because it will return a matrix. It may, however, be useful when using this function by itself, or as a probability generating function for the \code{\link{node_competing_events}} function.
  }
}
\details{
This function works essentially like the \code{\link{node_binomial}} function. First, the matrix of \code{betas} coefficients is used in conjunction with the values defined in the \code{parents} nodes and the \code{intercepts} to calculate the expected subject-specific probabilities of occurence for each possible category. This is done using the standard multinomial regression equations. Using those probabilities in conjunction with the \code{\link{rcategorical}} function, a single one of the possible categories is drawn for each individual.

Since this function produces categorical output (as it should), it may be difficult to use this node type as a parent for other nodes. Nevertheless, it is of course possible using a user-defined node type (see \code{\link{node_custom}} for some infos on how to define those).
}
\author{
Robin Denz
}
\examples{
library(simDAG)

set.seed(3345235)

dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("UICC", type="multinomial", parents=c("sex", "age"),
       betas=matrix(c(0.2, 0.4, 0.1, 0.5, 1.1, 1.2), ncol=2),
       intercepts=1)

sim_dat <- sim_from_dag(dag=dag, n_sim=100)
}
