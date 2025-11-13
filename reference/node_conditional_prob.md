# Generate Data Using Conditional Probabilities

This function can be used to generate dichotomous or categorical
variables dependent on one or more categorical variables where the
probabilities of occurrence in each strata defined by those variables is
known.

## Usage

``` r
node_conditional_prob(data, parents, probs, default_probs=NULL,
                      default_val=NA, labels=NULL,
                      coerce2factor=FALSE, check_inputs=TRUE)
```

## Arguments

- data:

  A `data.table` (or something that can be coerced to a `data.table`)
  containing all columns specified by `parents`.

- parents:

  A character vector specifying the names of the parents that this
  particular child node has.

- probs:

  A named list where each element corresponds to one stratum defined by
  parents. If only one name is given in `parents`, this means that there
  should be one element for possible value of the variable given in
  `parents`. If the node has multiple `parents`, there needs to be one
  element for possible combinations of `parents` (see examples). The
  values of those elements should either be a single number,
  corresponding to the probability of occurrence of a single event/value
  in case of a dichotomous variable, or a vector of probabilities that
  sum to 1, corresponding to class probabilities. In either case, the
  length of all elements should be the same. If possible strata of
  `parents` (or their possible combinations in case of multiple
  `parents`) are omitted, the result will be set to `default_val` for
  these omitted strata. See argument `default_val` and argument
  `default_probs` for an alternative.

- default_probs:

  If not all possible strata of `parents` are included in `probs`, the
  user may set default probabilities for all omitted strata. For
  example, if there are three strata (A, B and C) defined by `parents`
  and `probs` only contains defined probabilities for strata A, the
  probabilities for strata B and C can be set simultaneously by using
  this argument. Should be a single value between 0 and 1 for Bernoulli
  trials and a numeric vector with sum 1 for multinomial trials. If
  `NULL` (default) the value of the produced output for missing strata
  will be set to `default_val` (see below).

- default_val:

  Value of the produced variable in strata that are not included in the
  `probs` argument. If `default_probs` is not `NULL`, that arguments
  functionality will be used instead.

- labels:

  A vector of labels for the generated output. If `NULL` (default) and
  the output is dichotomous, a logical variable will be returned. If
  `NULL` and the output is categorical, it simply uses integers starting
  from 1 as class labels.

- coerce2factor:

  A single logical value specifying whether to return the drawn events
  as a factor or not.

- check_inputs:

  A single logical value specifying whether input checks should be
  performed or not. Set to `FALSE` to save some computation time in
  simulations.

## Details

Utilizing the user-defined discrete probability distribution in each
stratum of `parents` (supplied using the `probs` argument), this
function simply calls either the
[`rbernoulli`](https://robindenz1.github.io/simDAG/reference/rbernoulli.md)
or the
[`rcategorical`](https://robindenz1.github.io/simDAG/reference/rcategorical.md)
function.

***Formal Description***:

Formally, the data generation process can be described as a series of
conditional equations. For example, suppose that there is just one
parent node `sex` with the levels `male` and `female` with the goal of
creating a binary outcome that has a probability of occurrence of 0.5
for males and 0.7 for females. The conditional equation is then:

\$\$Y \sim Bernoulli(p),\$\$

where:

\$\$p = \begin{cases} 0.5, & \text{if } \texttt{sex="male"} \\ 0.7, &
\text{if } \texttt{sex="female"} \\ \end{cases},\$\$

and \\Bernoulli(p)\\ is the Bernoulli distribution with success
probability \\p\\. If the outcome has more than two categories, the
Bernoulli distribution would be replaced by \\Multinomial(p)\\ with
\\p\\ being replaced by a matrix of class probabilities. If there are
more than two variables, the conditional distribution would be
stratified by the intersection of all subgroups defined by the
variables.

An even more flexible node type, allowing arbitrary
[`node`](https://robindenz1.github.io/simDAG/reference/node.md)
definitions for different subsets of the previously generated data is
included in
[`node_mixture`](https://robindenz1.github.io/simDAG/reference/node_mixture.md).

## Author

Robin Denz

## Value

Returns a numeric vector of length `nrow(data)`.

## See also

[`empty_dag`](https://robindenz1.github.io/simDAG/reference/empty_dag.md),
[`node`](https://robindenz1.github.io/simDAG/reference/node.md),
[`node_td`](https://robindenz1.github.io/simDAG/reference/node.md),
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md),
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)

## Examples

``` r
library(simDAG)

set.seed(42)

#### two classes, one parent node ####

# define conditional probs
probs <- list(male=0.5, female=0.8)

# define DAG
dag <- empty_dag() +
  node("sex", type="rcategorical", labels=c("male", "female"),
       output="factor", probs=c(0.5, 0.5)) +
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
       output="factor", probs=c(0.5, 0.5)) +
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
       output="factor", probs=c(0.5, 0.5)) +
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
       output="factor", probs=c(0.5, 0.5)) +
  node("chemo", type="rbernoulli", p=0.5) +
  node("A", type="conditional_prob", parents=c("sex", "chemo"), probs=probs)

# generate data
data <- sim_from_dag(dag=dag, n_sim=1000)
```
