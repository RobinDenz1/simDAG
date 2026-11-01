# Generate Data from a Gamma Regression Model

Data from the parents is used to generate the node using gamma
regression by applying the betas to the design matrix and sampling from
the [`rgamma`](https://rdrr.io/r/stats/GammaDist.html) function.

## Usage

``` r
node_gamma(data, parents, formula=NULL, betas,
           intercept, shape, link="log")
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
  `parents=c("A", "B")` is equal to using `formula= ~ A + B`. Random
  effects and random slopes are currently not supported here.

- betas:

  A numeric vector with length equal to `parents`, specifying the causal
  beta coefficients used to generate the node.

- intercept:

  A single number specifying the intercept that should be used when
  generating the node.

- shape:

  A single positive number specifying the shape parameter (`shape`
  argument in [`rgamma`](https://rdrr.io/r/stats/GammaDist.html)). The
  shape parameter determines the conditional variance of the generated
  values relative to their conditional mean.

- link:

  The link function used to transform the linear predictor to the
  conditional mean used in
  [`rgamma`](https://rdrr.io/r/stats/GammaDist.html). For a standard
  gamma regression model, this should be set to `"log"` (which is the
  default). Other allowed values are `"identity"` and `"inverse"`.

## Details

This function uses the linear predictor defined by the `betas` and the
input design matrix to sample from a subject-specific gamma
distribution. It does so by calculating the linear predictor using the
`data`, `betas` and `intercept`, applying the inverse of the specified
link function to obtain the conditional mean, and passing it to the
`rgamma` function with `shape=shape` and `scale=mu_i/shape` of the stats
package.

For an observation with conditional mean \\\mu_i\\ and shape parameter
\\\alpha\\, the generated outcome follows a gamma distribution with
shape \\\alpha\\ and scale \\\mu_i/\alpha\\. Consequently, the
conditional mean is \\E(Y_i\mid X_i)=\mu_i\\ and the conditional
variance is \\\mathrm{Var}(Y_i\mid X_i)=\mu_i^2/\alpha\\.

Every non-missing conditional mean must be finite and strictly positive.
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
  node("cost", type="gamma", shape=3,
       formula= ~ 1.5 + age*0.01 + sexTRUE*0.2)

sim_dat <- sim_from_dag(dag=dag, n_sim=100, sort_dag=FALSE)
```
