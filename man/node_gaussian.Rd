\name{node_gaussian}
\alias{node_gaussian}

\title{
Generate Data from a (Mixed) Linear Regression Model
}
\description{
Data from the parents is used to generate the node using linear regression by predicting the covariate specific mean and sampling from a normal distribution with that mean and a specified standard deviation. Allows inclusion of arbitrary random effects and slopes.
}
\usage{
node_gaussian(data, parents, formula=NULL, betas, intercept, error,
              var_corr=NULL, link="identity")
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
  \item{error}{
A single number specifying the sigma error that should be used when generating the node. By setting this argument to 0, the linear predictor is returned directly. If \code{formula} contains mixed model syntax, this argument is passed to the \code{sigma} argument of the \code{\link[simr]{makeLmer}} function of the \pkg{simr} package.
  }
  \item{var_corr}{
Variances and covariances for random effects. Only used when \code{formula} contains mixed model syntax. If there are multiple random effects, their parameters should be supplied as a named list. More complex structures are also supported. This argument is directly passed to the \code{\link[simr]{makeLmer}} function of the \pkg{simr} package. Please consult the documentation of that package for more information on how mixed models should be specified. Some guidance can also be found in the "Issues" section of the official \pkg{simr} github page.
  }
  \item{link}{
The link function used to transform the linear predictor before adding the random error to it. For a standard linear regression model, this should be set to \code{link="identity"} (which is the default). Other allowed values are \code{"log"} and \code{"inverse"}, which are defined the same way as in the classic \code{\link[stats]{glm}} function.
  }
}
\details{
Using the general linear regression equation, the observation-specific value that would be expected given the model is generated for every observation in the dataset generated thus far. We could stop here, but this would create a perfect fit for the node, which is unrealistic. Instead, we add an error term by taking one sample of a normal distribution for each observation with mean zero and standard deviation \code{error}. This error term is then added to the predicted mean.

\strong{\emph{Formal Description}}:

Formally, the data generation can be described as:

\deqn{Y \sim \texttt{intercept} + \texttt{parents}_1 \cdot \texttt{betas}_1 + ... + \texttt{parents}_n \cdot \texttt{betas}_n+ N(0, \texttt{error}),}

where \eqn{N(0, \texttt{error})} denotes the normal distribution with mean 0 and a standard deviation of \code{error} and \eqn{n} is the number of parents (\code{length(parents)}).

For example, given \code{intercept=-15}, \code{parents=c("A", "B")}, \code{betas=c(0.2, 1.3)} and \code{error=2} the data generation process is defined as:

\deqn{Y \sim -15 + A \cdot 0.2 + B \cdot 1.3 + N(0, 2).}

When using a \code{link} other than \code{"identity"}, the procedure is equivalent, except that the link function is applied to the linear predictor before adding the random error term. For example, when using \code{link="log"}, \eqn{exp(-15 + A \cdot 0.2 + B \cdot 1.3) + N(0, 2)} is used instead.

\strong{\emph{Random Effects and Random Slopes}}:

This function also allows users to include arbitrary amounts of random slopes and random effects using the \code{formula} argument. If this is done, the \code{formula}, and \code{data} arguments are passed to the variables of the same name in the \code{\link[simr]{makeLmer}} function of the \pkg{simr} package. The \code{fixef} argument of that function will be passed the numeric vector \code{c(intercept, betas)} and the \code{VarCorr} argument receives the \code{var_corr} argument as input. If used as a node type in a \code{DAG}, all of this is taken care of behind the scenes. Users can simply use the regular enhanced formula interface of the \code{\link{node}} function to define these formula terms, as shown in detail in the formula vignette (\code{vignette(topic="v_using_formulas", package="simDAG")}). Please consult that vignette for examples. Also, please note that inclusion of random effects or random slopes usually results in significantly longer computation times.

}
\author{
Robin Denz
}
\value{
Returns a numeric vector of length \code{nrow(data)}.
}
\seealso{
\code{\link{empty_dag}}, \code{\link{node}}, \code{\link{node_td}}, \code{\link{sim_from_dag}}, \code{\link{sim_discrete_time}}
}
\examples{
library(simDAG)

set.seed(12455432)

# define a DAG
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("bmi", type="gaussian", parents=c("sex", "age"),
       betas=c(1.1, 0.4), intercept=12, error=2)

# define the same DAG, but with a pretty formula for the child node
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("bmi", type="gaussian", error=2,
       formula= ~ 12 + sexTRUE*1.1 + age*0.4)

sim_dat <- sim_from_dag(dag=dag, n_sim=100)

## an example using a random effect
if (requireNamespace("simr")) {

library(simr)

dag_mixed <- empty_dag() +
  node("School", type="rcategorical", probs=rep(0.1, 10),
       labels=LETTERS[1:10]) +
  node("Age", type="rnorm", mean=12, sd=2) +
  node("Grade", type="gaussian", formula= ~ -2 + Age*1.2 + (1|School),
       var_corr=0.3, error=1)

sim_dat <- sim_from_dag(dag=dag_mixed, n_sim=20)
}
}
