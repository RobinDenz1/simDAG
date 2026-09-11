\name{node_poisson}
\alias{node_poisson}

\title{
Generate Data from a (Mixed) Poisson Regression Model
}
\description{
Data from the parents is used to generate the node using poisson regression by predicting the covariate specific lambda and sampling from a poisson distribution accordingly. Allows inclusion of arbitrary random effects and slopes.
}
\usage{
node_poisson(data, parents, formula=NULL, betas, intercept,
             var_corr=NULL, link="log")
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
  \item{var_corr}{
Variances and covariances for random effects. Only used when \code{formula} contains mixed model syntax. If there are multiple random effects, their parameters should be supplied as a named list. More complex structures are also supported. This argument is directly passed to the \code{\link[simr]{makeLmer}} function of the \pkg{simr} package. Please consult the documentation of that package for more information on how mixed models should be specified. Some guidance can also be found in the "Issues" section of the official \pkg{simr} github page.
  }
  \item{link}{
The link function used to transform the linear predictor to the \code{lambda} value used in \code{\link[stats]{rpois}}. For a standard Poisson regression model, this should be set to \code{"log"} (which is the default). Other allowed values are \code{"identity"} and \code{"sqrt"}, which are defined the same way as in the classic \code{\link[stats]{glm}} function.
  }
}
\details{
Essentially, this function simply calculates the linear predictor defined by the \code{betas}-coefficients, the \code{intercept} and the values of the \code{parents}. The \code{link} function is then applied to this predictor and the result is passed to the \code{\link[stats]{rpois}} function. The result is a draw from a subject-specific poisson distribution, resembling the user-defined poisson regression model.

\strong{\emph{Formal Description}}:

Formally, the data generation (using \code{link="log"}) can be described as:

\deqn{Y \sim Poisson(\lambda),}

where \eqn{Poisson()} means that the variable is Poisson distributed with:

\deqn{P_\lambda(k) = \frac{\lambda^k e^{-\lambda}}{k!}.}

Here, \eqn{k} is the count and \eqn{e} is eulers number. The parameter \eqn{\lambda} is determined as:

\deqn{\lambda = \exp(\texttt{intercept} + \texttt{parents}_1 \cdot \texttt{betas}_1 + ... + \texttt{parents}_n \cdot \texttt{betas}_n),}

where \eqn{n} is the number of parents (\code{length(parents)}).

For example, given \code{intercept=-15}, \code{parents=c("A", "B")}, \code{betas=c(0.2, 1.3)} the data generation process is defined as:

\deqn{Y \sim Poisson(\exp(-15 + A \cdot 0.2 + B \cdot 1.3)).}

\strong{\emph{Random Effects and Random Slopes}}:

This function also allows users to include arbitrary amounts of random slopes and random effects using the \code{formula} argument. If this is done, the \code{formula}, and \code{data} arguments are passed to the variables of the same name in the \code{\link[simr]{makeGlmer}} function of the \pkg{simr} package. The \code{fixef} argument of that function will be passed the numeric vector \code{c(intercept, betas)} and the \code{VarCorr} argument receives the \code{var_corr} argument as input. If used as a node type in a \code{DAG}, all of this is taken care of behind the scenes. Users can simply use the regular enhanced formula interface of the \code{\link{node}} function to define these formula terms, as shown in detail in the formula vignette (\code{vignette(topic="v_using_formulas", package="simDAG")}). Please consult that vignette for examples. Also, please note that inclusion of random effects or random slopes usually results in significantly longer computation times.

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

set.seed(345345)

dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("smoking", type="poisson",
       formula= ~ -2 + sexTRUE*1.1 + age*0.4)

sim_dat <- sim_from_dag(dag=dag, n_sim=100)

## an example using a random effect
if (requireNamespace("simr")) {

library(simr)

dag_mixed <- empty_dag() +
  node("School", type="rcategorical", probs=rep(0.1, 10),
       labels=LETTERS[1:10]) +
  node("Age", type="rnorm", mean=12, sd=2) +
  node("Grade", type="poisson", formula= ~ -2 + Age*1.2 + (1|School),
       var_corr=0.3)

sim_dat <- sim_from_dag(dag=dag_mixed, n_sim=20)
}

}
