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
                 labels=NULL, output="factor",
                 return_prob=FALSE)
}
\arguments{
  \item{data}{
A \code{data.table} (or something that can be coerced to a \code{data.table}) containing all columns specified by \code{parents}.
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
  \item{output}{
A single character string specifying the output format. Must be one of \code{"factor"} (default), \code{"character"} or \code{"numeric"}. If the argument \code{labels} is supplied, the output will coerced to \code{"character"} by default.
  }
  \item{return_prob}{
Either \code{TRUE} or \code{FALSE} (default). Specifies whether to return the matrix of class probabilities or not. If you are using this function inside of a \code{\link{node}} call, you cannot set this to \code{TRUE} because it will return a matrix. It may, however, be useful when using this function by itself, or as a probability generating function for the \code{\link{node_competing_events}} function.
  }
}
\details{
This function works essentially like the \code{\link{node_binomial}} function. First, the matrix of \code{betas} coefficients is used in conjunction with the values defined in the \code{parents} nodes and the \code{intercepts} to calculate the expected subject-specific probabilities of occurrence for each possible category. This is done using the standard multinomial regression equations. Using those probabilities in conjunction with the \code{\link{rcategorical}} function, a single one of the possible categories is drawn for each individual.

When actually fitting a multinomial regression model (with functions such as \code{multinom} from the \pkg{nnet} package), the coefficients will usually not be equal to the ones supplied in \code{betas}. The reason is that these functions usually standardize the coefficients to the coefficient of the reference category.
}
\author{
Robin Denz
}
\value{
Returns a vector of length \code{nrow(data)}. Depending on the used arguments, this vector may be of type character, numeric of factor. If \code{return_prob} was used it instead returns a numeric matrix containing one column per possible event and \code{nrow(data)} rows.
}
\seealso{
\code{\link{empty_dag}}, \code{\link{node}}, \code{\link{node_td}}, \code{\link{sim_from_dag}}, \code{\link{sim_discrete_time}}
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
