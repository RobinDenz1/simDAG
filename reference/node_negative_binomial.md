# Generate Data from a Negative Binomial Regression Model

Data from the parents is used to generate the node using negative
binomial regression by applying the betas to the design matrix and
sampling from the `rnbinom` function.

## Usage

``` r
node_negative_binomial(data, parents, formula=NULL, betas,
                       intercept, theta, link="log")
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

- intercept:

  A single number specifying the intercept that should be used when
  generating the node.

- theta:

  A single number specifying the theta parameter (`size` argument in
  `rnbinom`).

- link:

  The link function used to transform the linear predictor to the `mu`
  value used in [`rnbinom`](https://rdrr.io/r/stats/NegBinomial.html).
  For a standard negative binomial regression model, this should be set
  to `"log"` (which is the default). Other allowed values are
  `"identity"` and `"sqrt"`.

## Details

This function uses the linear predictor defined by the `betas` and the
input design matrix to sample from a subject-specific negative binomial
distribution. It does to by calculating the linear predictor using the
`data`, `betas` and `intercept`, applying the inverse of the link
function to it and passing it to the `mu` argument of the `rnbinom`
function of the stats package.

This node type currently does not support inclusion of random effects or
random slopes in the `formula`.

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

set.seed(124554)

dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("smoking", type="negative_binomial", theta=0.05,
       formula= ~ -2 + sexTRUE*1.1 + age*0.4)

sim_dat <- sim_from_dag(dag=dag, n_sim=100, sort_dag=FALSE)
```
