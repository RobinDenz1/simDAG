\name{node_binomial}
\alias{node_binomial}

\title{
Simulate a Node Using Logistic Regression
}
\description{
Data from the parents is used to generate the node using logistic regression by predicting the covariate specific probability of 1 and sampling from a Bernoulli distribution accordingly.
}
\usage{
node_binomial(data, parents, formula=NULL, betas, intercept,
              return_prob=FALSE, output="logical", labels=NULL,
              var_corr=NULL)
}
\arguments{
  \item{data}{
A \code{data.table} (or something that can be coerced to a \code{data.table}) containing all columns specified by \code{parents}.
  }
  \item{parents}{
A character vector specifying the names of the parents that this particular child node has. If non-linear combinations or interaction effects should be included, the user may specify the \code{formula} argument instead.
  }
  \item{formula}{
An optional \code{formula} object to describe how the node should be generated or \code{NULL} (default). If supplied it should start with \code{~}, having nothing else on the left hand side. The right hand side may contain any valid formula syntax, such as \code{A + B} or \code{A + B + I(A^2)}, allowing non-linear effects. If this argument is defined, there is no need to define the \code{parents} argument. For example, using \code{parents=c("A", "B")} is equal to using \code{formula= ~ A + B}. May contain random effects and random slopes, in which case the \pkg{simr} package is used to generate the data. See details.
  }
  \item{betas}{
A numeric vector with length equal to \code{parents}, specifying the causal beta coefficients used to generate the node.
  }
  \item{intercept}{
A single number specifying the intercept that should be used when generating the node.
  }
  \item{return_prob}{
Either \code{TRUE} or \code{FALSE} (default). If \code{TRUE}, the calculated probability is returned instead of the results of bernoulli trials. This argument is ignored if random effects or random slopes are specified in the \code{formula} input.
  }
  \item{output}{
A single character string, must be either \code{"logical"} (default), \code{"numeric"}, \code{"character"} or \code{"factor"}. If \code{output="character"} or \code{output="factor"}, the labels (or levels in case of a factor) can be set using the \code{labels} argument.
  }
  \item{labels}{
A character vector of length 2 or \code{NULL} (default). If \code{NULL}, the resulting vector is returned as is. If a character vector is supplied and \code{output="character"} or \code{output="factor"} is used, all \code{TRUE} values are replaced by the first entry of this vector and all \code{FALSE} values are replaced by the second argument of this vector. The output will then be a character variable or factor variable, depending on the \code{output} argument. This argument is ignored if \code{output} is set to \code{"numeric"} or \code{"logical"}.
  }
  \item{var_corr}{
Variances and covariances for random effects. Only used when \code{formula} contains mixed model syntax. If there are multiple random effects, their parameters should be supplied as a named list. This argument is directly passed to the \code{\link[simr]{makeLmer}} function of the \pkg{simr} package. Please consult the documentation of that package for more information on how mixed models should be specified.
  }
}
\details{
Using the normal form a logistic regression model, the observation specific event probability is generated for every observation in the dataset. Using the \code{rbernoulli} function, this probability is then used to take one bernoulli sample for each observation in the dataset. If only the probability should be returned \code{return_prob} should be set to \code{TRUE}.

\strong{\emph{Formal Description}}:

Formally, the data generation can be described as:

\deqn{Y \sim Bernoulli(logit(\texttt{intercept} + \texttt{parents}_1 \cdot \texttt{betas}_1 + ... + \texttt{parents}_n \cdot \texttt{betas}_n)),}

where \eqn{Bernoulli(p)} denotes one Bernoulli trial with success probability \eqn{p}, \eqn{n} is the number of parents (\code{length(parents)}) and the \eqn{logit(x)} function is defined as:

\deqn{logit(x) = ln(\frac{x}{1-x}).}

For example, given \code{intercept=-15}, \code{parents=c("A", "B")} and \code{betas=c(0.2, 1.3)} the data generation process is defined as:

\deqn{Y \sim Bernoulli(logit(-15 + A \cdot 0.2 + B \cdot 1.3)).}

\strong{\emph{Output Format}}:

By default this function returns a logical vector containing only \code{TRUE} and \code{FALSE} entries, where \code{TRUE} corresponds to an event and \code{FALSE} to no event. This may be changed by using the \code{output} and \code{labels} arguments. The last three arguments of this function are ignored if \code{return_prob} is set to \code{TRUE}.

\strong{\emph{Random Effects and Random Slopes}}:

This function also allows users to arbitrary amounts of random slopes and random effects using the \code{formula} argument. If this is done, the \code{formula}, and \code{data} arguments are passed to the variables of the same name in the \code{\link[simr]{makeGlmer}} function of the \pkg{simr} package. The \code{fixef} argument of that function will be passed the numeric vector \code{c(intercept, betas)} and the \code{VarCorr} argument receives the \code{var_corr} argument as input. If used as a node type in a \code{DAG}, all of this is taken care of behind the scenes. Users can simply use the regular enhanced formula interface of the \code{\link{node}} function to define these formula terms, as shown in detail in the formula vignette (\code{vignette(topic="v_using_formulas", package="simDAG")}). Please consult that vignette for examples. Also, please note that inclusion of random effects or random slopes usually results in significantly longer computation times.

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

# define the same DAG, but using a pretty formula
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("smoking", type="binomial",
       formula= ~ -2 + age*1.1 + sexTRUE*0.4)

# simulate data from it
sim_dat <- sim_from_dag(dag=dag, n_sim=100)

# returning only the estimated probability instead
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("smoking", type="binomial", parents=c("age", "sex"),
       betas=c(1.1, 0.4), intercept=-2, return_prob=TRUE)

sim_dat <- sim_from_dag(dag=dag, n_sim=100)

## an example using a random effect
if (requireNamespace("simr")) {

library(simr)

dag_mixed <- empty_dag() +
  node("School", type="rcategorical", probs=rep(0.1, 10),
       labels=LETTERS[1:10]) +
  node("Age", type="rnorm", mean=12, sd=2) +
  node("Grade", type="binomial", formula= ~ -10 + Age*1.2 + (1|School),
       var_corr=0.3)

sim_dat <- sim_from_dag(dag=dag_mixed, n_sim=100)
}
}
