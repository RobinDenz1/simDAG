\name{node_conditional_distr}
\alias{node_conditional_distr}

\title{
Simulate a Node by Sampling from Different Distributions based on Strata
}
\description{
This function can be used to generate any kind of dichotomous, categorical or numeric variables dependent on one or more categorical variables by randomly sampling from user-defined distributions in each strata defined by the nodes parents.
}
\usage{
node_conditional_distr(data, parents, distr, default_distr=NULL,
                       default_distr_args=list(), default_val=NA_real_,
                       coerce2numeric=TRUE, check_inputs=TRUE)
}
\arguments{
  \item{data}{
A \code{data.table} (or something that can be coerced to a \code{data.table}) containing all columns specified by \code{parents}.
  }
  \item{parents}{
A character vector specifying the names of the parents that this particular child node has.
  }
  \item{distr}{
A named list where each element corresponds to one stratum defined by parents. If only one name is given in \code{parents}, this means that there should be one element for possible values of the variable given in \code{parents}. If the node has multiple \code{parents}, there needs to be one element for possible combinations of \code{parents} (see examples). The values of those elements should be a \code{list} themselves, with the first argument being a callable function (such as \code{rnorm}, \code{rcategorical}, ...) and the rest should be named arguments of that function. Any function can be used, as long as it returns a vector of \code{n} values, with \code{n} being an argument of the function. \code{n} is set internally based on the stratum size and cannot be set by the user. If this list does not contain one element for each possible strata defined by \code{parents}, the \code{default_val} or \code{default_distr} arguments will be used.
  }
  \item{default_distr}{
A function that should be used to generate values for all strata that are not explicitly mentioned in the \code{distr} argument, or \code{NULL} (default). If \code{NULL}, the \code{default_val} argument will be used to fill the missing strata with values. A  function passed to this argument should contain the argument \code{n}, which should define the number of samples to generate. It should return a vector with \code{n} values. Some examples are (again), \code{\link{rnorm}} or \code{\link{rbernoulli}}.
  }
  \item{default_distr_args}{
A named list of arguments which are passed to the function defined by the \code{default_distr} argument. Ignored if \code{default_distr} is \code{NULL}.
  }
  \item{default_val}{
A single value which is used as an output for strata that are not mentioned in \code{distr}. Ignored if \code{default_distr} is not \code{NULL}.
  }
  \item{coerce2numeric}{
A single logical value specifying whether to try to coerce the resulting variable to numeric or not.
  }
  \item{check_inputs}{
A single logical value specifying whether to perform input checks or not. May be set to \code{TRUE} to speed up things a little if you are sure your input is correct.
  }
}
\details{
Utilizing the user-defined distribution in each stratum of \code{parents} (supplied using the \code{distr} argument), this function simply calls the user-defined function with the arguments given by the user to generate a new variable. This allows the new variable to consist of a mix of different distributions, based on categorical \code{parents}.

\strong{\emph{Formal Description}}:

Formally, the data generation process can be described as a series of conditional equations. For example, suppose that there is just one parent node \code{sex} with the levels \code{male} and \code{female} with the goal of creating a continuous outcome that has a normal distribution of \eqn{N(10, 3)} for males and \eqn{N(7, 2)} for females. The conditional equation is then:

\deqn{Y \sim \begin{cases}
			N(10, 3), & \text{if } \texttt{sex="male"} \\
			N(7, 2), & \text{if } \texttt{sex="female"} \\
		\end{cases},}

If there are more than two variables, the conditional distribution would be stratified by the intersection of all subgroups defined by the variables.
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

#### with one parent node ####

# define conditional distributions
distr <- list(male=list("rnorm", mean=100, sd=5),
              female=list("rcategorical", probs=c(0.1, 0.2, 0.7)))

# define DAG
dag <- empty_dag() +
  node("sex", type="rcategorical", labels=c("male", "female"),
       output="factor", probs=c(0.4, 0.6)) +
  node("chemo", type="rbernoulli", p=0.5) +
  node("A", type="conditional_distr", parents="sex", distr=distr)

# generate data
data <- sim_from_dag(dag=dag, n_sim=1000)


#### with two parent nodes ####

# define conditional distributions with interaction between parents
distr <- list(male.FALSE=list("rnorm", mean=100, sd=5),
              male.TRUE=list("rnorm", mean=100, sd=20),
              female.FALSE=list("rbernoulli", p=0.5),
              female.TRUE=list("rcategorical", probs=c(0.1, 0.2, 0.7)))

# define DAG
dag <- empty_dag() +
  node("sex", type="rcategorical", labels=c("male", "female"),
       output="factor", probs=c(0.4, 0.6)) +
  node("chemo", type="rbernoulli", p=0.5) +
  node("A", type="conditional_distr", parents=c("sex", "chemo"), distr=distr)

# generate data
data <- sim_from_dag(dag=dag, n_sim=1000)
}
