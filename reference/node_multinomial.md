# Generate Data from a Multinomial Regression Model

Data from the parents is used to generate the node using multinomial
regression by predicting the covariate specific probability of each
class and sampling from a multinomial distribution accordingly.

## Usage

``` r
node_multinomial(data, parents, betas, intercepts,
                 labels=NULL, output="factor",
                 return_prob=FALSE)
```

## Arguments

- data:

  A `data.table` (or something that can be coerced to a `data.table`)
  containing all columns specified by `parents`.

- parents:

  A character vector specifying the names of the parents that this
  particular child node has.

- betas:

  A numeric matrix with `length(parents)` columns and one row for each
  class that should be simulated, specifying the causal beta
  coefficients used to generate the node.

- intercepts:

  A numeric vector with one entry for each class that should be
  simulated, specifying the intercepts used to generate the node.

- labels:

  An optional character vector giving the factor levels of the generated
  classes. If `NULL` (default), the integers are simply used as factor
  levels.

- output:

  A single character string specifying the output format. Must be one of
  `"factor"` (default), `"character"` or `"numeric"`. If the argument
  `labels` is supplied, the output will coerced to `"character"` by
  default.

- return_prob:

  Either `TRUE` or `FALSE` (default). Specifies whether to return the
  matrix of class probabilities or not. If you are using this function
  inside of a
  [`node`](https://robindenz1.github.io/simDAG/reference/node.md) call,
  you cannot set this to `TRUE` because it will return a matrix. It may,
  however, be useful when using this function by itself, or as a
  probability generating function for the
  [`node_competing_events`](https://robindenz1.github.io/simDAG/reference/node_competing_events.md)
  function.

## Details

This function works essentially like the
[`node_binomial`](https://robindenz1.github.io/simDAG/reference/node_binomial.md)
function. First, the matrix of `betas` coefficients is used in
conjunction with the values defined in the `parents` nodes and the
`intercepts` to calculate the expected subject-specific probabilities of
occurrence for each possible category. This is done using the standard
multinomial regression equations. Using those probabilities in
conjunction with the
[`rcategorical`](https://robindenz1.github.io/simDAG/reference/rcategorical.md)
function, a single one of the possible categories is drawn for each
individual.

When actually fitting a multinomial regression model (with functions
such as `multinom` from the nnet package), the coefficients will usually
not be equal to the ones supplied in `betas`. The reason is that these
functions usually standardize the coefficients to the coefficient of the
reference category.

## Author

Robin Denz

## Value

Returns a vector of length `nrow(data)`. Depending on the used
arguments, this vector may be of type character, numeric of factor. If
`return_prob` was used it instead returns a numeric matrix containing
one column per possible event and `nrow(data)` rows.

## See also

[`empty_dag`](https://robindenz1.github.io/simDAG/reference/empty_dag.md),
[`node`](https://robindenz1.github.io/simDAG/reference/node.md),
[`node_td`](https://robindenz1.github.io/simDAG/reference/node.md),
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md),
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)

## Examples

``` r
library(simDAG)

set.seed(3345235)

dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("UICC", type="multinomial", parents=c("sex", "age"),
       betas=matrix(c(0.2, 0.4, 0.1, 0.5, 1.1, 1.2), ncol=2),
       intercepts=1)

sim_dat <- sim_from_dag(dag=dag, n_sim=100)
```
