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
Either \code{TRUE} or \code{FALSE} (default). Specifies whether to return the matrix of class probabilities or not.
  }
}
\details{
More explanation
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
