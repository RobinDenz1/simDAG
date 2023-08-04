\name{node_binomial}
\alias{node_binomial}

\title{
Simulate a Node Using Logistic Regression
}
\description{
Data from the parents is used to generate the node using logistic regression by predicting the covariate specific probability of 1 and sampling from a bernoulli distribution accordingly.
}
\usage{
node_binomial(data, parents, formula=NULL, betas, intercept,
              return_prob=FALSE, coerce2factor=FALSE,
              coerce2numeric=FALSE, labels=NULL)
}
\arguments{
  \item{data}{
A \code{data.frame} containing all columns specified by \code{parents}.
  }
  \item{parents}{
A character vector specifying the names of the parents that this particular child node has. If non-linear combinations or interaction effects should be included, the user may specify the \code{formula} argument instead.
  }
  \item{formula}{
An optional \code{formula} object to describe how the node should be generated or \code{NULL} (default). If supplied it should start with \code{~}, having nothing else on the left hand side. The right hand side may contain any valid formula syntax, such as \code{A + B} or \code{A + B + I(A^2)}, allowing non-linear effects. If this argument is defined, there is no need to define the \code{parents} argument. For example, using \code{parents=c("A", "B")} is equal to using \code{formula= ~ A + B}.
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
  \item{coerce2factor}{
Either \code{TRUE} or \code{FALSE} (default). If \code{TRUE}, the resulting vector is coerced to a factor variable. Levels of this factor can be set using the \code{labels} argument.
  }
  \item{coerce2numeric}{
Either \code{TRUE} or \code{FALSE} (default). If \code{TRUE}, the resulting vector is coerced to a numeric variable (0/1).
  }
  \item{labels}{
A character vector of length 2 or \code{NULL} (default). If \code{NULL}, the resulting vector is returned as is. If a character vector is supplied all \code{TRUE} values are replaced by the first entry of this vector and all \code{FALSE} values are replaced by the second argument of this vector. The output will then be a character variable, unless \code{coerce2factor} is set to \code{TRUE} in which case it will be a factor variable.
  }
}
\details{
Using the normal form a logistic regression model, the observation specific event probability is generated for every observation in the dataset. Using the \code{rbernoulli} function, this probability is then used to take one bernoulli sample for each observation in the dataset. If only the probability should be returned \code{return_prob} should be set to \code{TRUE}.

By default this function returns a logical vector containing only \code{TRUE} and \code{FALSE} entries, where \code{TRUE} corresponds to an event and \code{FALSE} to no event. If those should be coded as 0/1 instead, the user can use the \code{coerce2numeric} argument. If they should be coded as a character with specific labels, the user can use the \code{labels} argument. To additionally output it as a factor, the user may use the \code{coerce2factor} argument. If both \code{coerce2factor} and \code{coerce2numeric} are set to \code{TRUE}, the result will be a factor. The last three arguments of this function are ignored if \code{return_prob} is set to \code{TRUE}.
}
\author{
Robin Denz
}
\value{
Returns a logical vector (or numeric vector if \code{return_prob=TRUE}) of length \code{nrow(data)}.
}
\seealso{
\code{\link{empty_dag}}, \code{\link{node}}, \code{\link{node_td}}, \code{\link{sim_from_dag}}, \code{\link{sim_discrete_time}}
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
