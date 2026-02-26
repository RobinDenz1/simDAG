# Generate Data from an Ordered Logistic or Probit Regression

Data from the parents is used to generate the node using by first
calculating the linear predictor. An appropriately distributed random
error term is then added. The resulting latent variable is categorized
at user-specified cutpoints to obtain the resulting ordered factor.

## Usage

``` r
node_polr(data, parents, formula=NULL, betas,
          cutpoints, link="logistic", labels=FALSE,
          output="factor")
```

## Arguments

- data:

  A `data.table` (or something that can be coerced to a `data.table`)
  containing all columns specified by `parents`.

- parents:

  A character vector specifying the names of the parents that this
  particular child node has. If non-linear combinations or interaction
  effects should be included, the user may specify the `formula`
  argument instead.

- formula:

  An optional `formula` object to describe how the node should be
  generated or `NULL` (default). If supplied it should start with `~`,
  having nothing else on the left hand side. The right hand side may
  contain any valid formula syntax, such as `A + B` or `A + B + I(A^2)`,
  allowing non-linear effects. If this argument is defined, there is no
  need to define the `parents` argument. For example, using
  `parents=c("A", "B")` is equal to using `formula= ~ A + B`. Contrary
  to the
  [`node_gaussian`](https://robindenz1.github.io/simDAG/reference/node_gaussian.md),
  [`node_binomial`](https://robindenz1.github.io/simDAG/reference/node_binomial.md)
  and
  [`node_poisson`](https://robindenz1.github.io/simDAG/reference/node_poisson.md)
  node types, random effects and random slopes are currently not
  supported here.

- betas:

  A numeric vector with length equal to `parents`, specifying the causal
  beta coefficients used to generate the node.

- cutpoints:

  A numeric vector containing the points at which the underlying latent
  variable is cut to produce the ordered factor. In general,
  `length(cutpoints) + 1` categories will be produced.

- link:

  The link function describing the distribution of the error term.
  Allowed values are `"logistic"`, `"probit"`, `"loglog"`, `"cloglog"`
  and `"cauchit"`, mirroring the `method` argument in the
  [`polr`](https://rdrr.io/pkg/MASS/man/polr.html) function.

- labels:

  Labels for the levels of the resulting categories. By default
  (`labels = FALSE`), simple integer codes are returned. Corresponds to
  the argument of the same name in the
  [`cut`](https://rdrr.io/r/base/cut.html) function.

- output:

  A single character string specifying the kind of output that should be
  returned. Defaults to `"factor"`, but may also be set to
  `"character"`.

## Details

This node type currently does not support inclusion of random effects or
random slopes in the `formula`.

## Author

Robin Denz

## Value

Returns a character of factor vector of length `nrow(data)`.

## See also

[`empty_dag`](https://robindenz1.github.io/simDAG/reference/empty_dag.md),
[`node`](https://robindenz1.github.io/simDAG/reference/node.md),
[`node_td`](https://robindenz1.github.io/simDAG/reference/node.md),
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md),
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)

## Examples

``` r
library(simDAG)

set.seed(124554)

dag <- empty_dag() +
  node(c("X1", "X2"), type="rnorm", mean=0, sd=1) +
  node("Y", type="polr", formula= ~ -2*X1 + 1.5*X2, cutpoints=c(0.7, 1.5, 3.2),
       link="logistic")

sim_dat <- sim_from_dag(dag=dag, n_sim=100)
```
