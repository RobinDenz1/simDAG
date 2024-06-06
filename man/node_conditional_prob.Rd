\name{node_conditional_prob}
\alias{node_conditional_prob}

\title{
Simulate a Node Using Conditional Probabilities
}
\description{
This function can be used to generate dichotomous or categorical variables dependent on one or more categorical variables where the probabilities of occurrence in each strata defined by those variables is known.
}
\usage{
node_conditional_prob(data, parents, probs, default_probs=NULL,
                      default_val=NA, labels=NULL,
                      coerce2factor=FALSE, check_inputs=TRUE)
}
\arguments{
  \item{data}{
A \code{data.frame} containing all columns specified by \code{parents}.
  }
  \item{parents}{
A character vector specifying the names of the parents that this particular child node has.
  }
  \item{probs}{
A named list where each element corresponds to one stratum defined by parents. If only one name is given in \code{parents}, this means that there should be one element for possible value of the variable given in \code{parents}. If the node has multiple \code{parents}, there needs to be one element for possible combinations of \code{parents} (see examples). The values of those elements should either be a single number, corresponding to the probability of occurrence of a single event/value in case of a dichotomous variable, or a vector of probabilities that sum to 1, corresponding to class probabilities. In either case, the length of all elements should be the same. If possible strata of \code{parents} (or their possible combinations in case of multiple \code{parents}) are omitted, the result will be set to \code{default_val} for these omitted strata. See argument \code{default_val} and argument \code{default_probs} for an alternative.
  }
  \item{default_probs}{
If not all possible strata of \code{parents} are included in \code{probs}, the user may set default probabilities for all omitted strata. For example, if there are three strata (A, B and C) defined by \code{parents} and \code{probs} only contains defined probabilities for strata A, the probabilities for strata B and C can be set simultaneously by using this argument. Should be a single value between 0 and 1 for Bernoulli trials and a numeric vector with sum 1 for multinomial trials. If \code{NULL} (default) the value of the produced output for missing strata will be set to \code{default_val} (see below).
  }
  \item{default_val}{
Value of the produced variable in strata that are not included in the \code{probs} argument. If \code{default_probs} is not \code{NULL}, that arguments functionality will be used instead.
  }
  \item{labels}{
A vector of labels for the generated output. If \code{NULL} (default) and the output is dichotomous, a logical variable will be returned. If \code{NULL} and the output is categorical, it simply uses integers starting from 1 as class labels.
  }
  \item{coerce2factor}{
A single logical value specifying whether to return the drawn events as a factor or not.
  }
  \item{check_inputs}{
A single logical value specifying whether input checks should be performed or not. Set to \code{FALSE} to save some computation time in simulations.
  }
}
\details{
Utilizing the user-defined discrete probability distribution in each stratum of \code{parents} (supplied using the \code{probs} argument), this function simply calls either the \code{\link{rbernoulli}} or the \code{\link{rcategorical}} function.

\strong{\emph{Formal Description}}:

Formally, the data generation process can be described as a series of conditional equations. For example, suppose that there is just one parent node \code{sex} with the levels \code{male} and \code{female} with the goal of creating a binary outcome that has a probability of occurence of 0.5 for males and 0.7 for females. The conditional equation is then:

\deqn{Y \sim Bernoulli(p),}

where:

\deqn{p = \begin{cases}
			0.5, & \text{if } \texttt{sex="male"} \\
			0.7, & \text{if } \texttt{sex="female"} \\
		\end{cases},}

and \eqn{Bernoulli(p)} is the Bernoulli distribution with success probability \eqn{p}. If the outcome has more than two categories, the Bernoulli distribution would be replaced by \eqn{Multinomial(p)} with \eqn{p} being replaced by a matrix of class probabilities. If there are more than two variables, the conditional distribution would be stratified by the intersection of all subgroups defined by the variables.

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

set.seed(42)

#### two classes, one parent node ####

# define conditional probs
probs <- list(male=0.5, female=0.8)

# define DAG
dag <- empty_dag() +
  node("sex", type="rcategorical", labels=c("male", "female"),
       coerce2factor=TRUE, probs=c(0.5, 0.5)) +
  node("chemo", type="rbernoulli", p=0.5) +
  node("A", type="conditional_prob", parents="sex", probs=probs)

# generate data
data <- sim_from_dag(dag=dag, n_sim=1000)


#### three classes, one parent node ####

# define conditional probs
probs <- list(male=c(0.5, 0.2, 0.3), female=c(0.8, 0.1, 0.1))

# define DAG
dag <- empty_dag() +
  node("sex", type="rcategorical", labels=c("male", "female"),
       coerce2factor=TRUE, probs=c(0.5, 0.5)) +
  node("chemo", type="rbernoulli", p=0.5) +
  node("A", type="conditional_prob", parents="sex", probs=probs)

# generate data
data <- sim_from_dag(dag=dag, n_sim=1000)


#### two classes, two parent nodes ####

# define conditional probs
probs <- list(male.FALSE=0.5,
              male.TRUE=0.8,
              female.FALSE=0.1,
              female.TRUE=0.3)

# define DAG
dag <- empty_dag() +
  node("sex", type="rcategorical", labels=c("male", "female"),
       coerce2factor=TRUE, probs=c(0.5, 0.5)) +
  node("chemo", type="rbernoulli", p=0.5) +
  node("A", type="conditional_prob", parents=c("sex", "chemo"), probs=probs)

# generate data
data <- sim_from_dag(dag=dag, n_sim=1000)


#### three classes, two parent nodes ####

# define conditional probs
probs <- list(male.FALSE=c(0.5, 0.1, 0.4),
              male.TRUE=c(0.8, 0.1, 0.1),
              female.FALSE=c(0.1, 0.7, 0.2),
              female.TRUE=c(0.3, 0.4, 0.3))

# define dag
dag <- empty_dag() +
  node("sex", type="rcategorical", labels=c("male", "female"),
       coerce2factor=TRUE, probs=c(0.5, 0.5)) +
  node("chemo", type="rbernoulli", p=0.5) +
  node("A", type="conditional_prob", parents=c("sex", "chemo"), probs=probs)

# generate data
data <- sim_from_dag(dag=dag, n_sim=1000)
}
