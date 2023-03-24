\name{node_conditional_distr}
\alias{node_conditional_distr}

\title{
Simulate a Node by Sampling from Different Distributions based on Strata
}
\description{
This function can be used to generate any kind of dichotomous, categorical or numeric variables dependent on one or more categorical variables by randomly sampling from user-defined distributions in each strata defined by the nodes parents.
}
\usage{
node_conditional_distr(data, parents, distr, coerce2numeric=TRUE)
}
\arguments{
  \item{data}{
A \code{data.frame} containing all columns specified by \code{parents}.
  }
  \item{parents}{
A character vector specifying the names of the parents that this particular child node has.
  }
  \item{distr}{
A named list where each element corresponds to one stratum defined by parents. If only one name is given in \code{parents}, this means that there should be one element for each possible value of the variable given in \code{parents}. If the node has multiple \code{parents}, there needs to be one element for each possible combination of \code{parents} (see examples). The values of those elements should be a \code{list} themselves, with the first argument being a callable function (such as \code{rnorm}, \code{rcategorical}, ...) and the rest arguments being named arguments of that function. Any function can be used, as long as it returns a vector of \code{n} values, with \code{n} being an argument of the function. \code{n} is set internally based on the stratum size and cannot be set by the user.
  }
  \item{coerce2numeric}{
A single logical value specifying whether to try to coerce the resulting variable to numeric or not.
  }
}
\details{
Utilizing the user-defined distribution in each stratum of \code{parents} (supplied using the \code{distr} argument), this function simply calls the user-defined function with the arguments given by the user to generate a new variable. This allows the new variable to consist of a mix of different distributions, based on categorical \code{parents}.
}
\author{
Robin Denz
}
\examples{
\dontrun{
set.seed(42)

n_sim <- 1000000
data <- data.table(sex=sample(c("male", "female"), size=n_sim, replace=TRUE),
                   chemo=sample(c("yes", "no"), size=n_sim, replace=TRUE))

# one parent node
distr <- list(male=list("rnorm", mean=100, sd=5),
              female=list("rcategorical", probs=c(0.1, 0.2, 0.7)))

out <- node_conditional_distr(data=data, parents="sex", distr=distr)

# with interaction of two parents
distr <- list(male.no=list("rnorm", mean=100, sd=5),
              male.yes=list("rnorm", mean=100, sd=20),
              female.no=list("rbernoulli", p=0.5),
              female.yes=list("rcategorical", probs=c(0.1, 0.2, 0.7)))

out <- node_conditional_distr(data=data, parents=c("sex", "chemo"), distr=distr)
}
}
