# Generate Data from a Beta Regression Model

Data from the parents is used to generate the node using beta regression
by applying the betas to the design matrix, transforming the resulting
linear predictor to a conditional mean, and sampling from the
[`rbeta`](https://rdrr.io/r/stats/Beta.html) function.

## Usage

``` r
node_beta(data, parents, formula=NULL, betas,
          intercept, precision, link="logit")
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

  A numeric vector with length equal to the number of columns in the
  design matrix, specifying the causal beta coefficients used to
  generate the node.

- intercept:

  A single number specifying the intercept that should be used when
  generating the node.

- precision:

  A single finite, positive number specifying the precision parameter
  \\\phi\\ of the beta distribution. The precision is the sum of the two
  shape parameters and determines the conditional variance of the
  generated values relative to their conditional mean.

- link:

  The link function used to transform the linear predictor to the
  conditional mean. The default is `"logit"`. Other allowed values are
  `"probit"`, `"cloglog"`, `"loglog"`, and `"cauchit"`.

## Details

This function uses the linear predictor defined by the `betas` and the
input design matrix to obtain an observation-specific conditional mean.
It does so by calculating the linear predictor using the `data`,
`betas`, and `intercept`, applying the inverse of the specified link
function, and passing the resulting conditional mean to the
[`rbeta`](https://rdrr.io/r/stats/Beta.html) function of the stats
package.

For an observation with conditional mean \\\mu_i\\ and precision
parameter \\\phi\\, the generated outcome follows a beta distribution
with shape parameters \\\mu_i\phi\\ and \\(1-\mu_i)\phi\\. Consequently,
the conditional mean is \\E(Y_i\mid X_i)=\mu_i\\ and the conditional
variance is \\\mathrm{Var}(Y_i\mid X_i)=\mu_i(1-\mu_i)/(\phi+1)\\.

Every non-missing conditional mean must be finite and strictly between
zero and one. This node type currently does not support inclusion of
random effects or random slopes in the `formula`. The precision
parameter is fixed across observations.

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
  node("proportion", type="beta", precision=10,
       formula= ~ -1 + age*0.01 + sexTRUE*0.2)

sim_dat <- sim_from_dag(dag=dag, n_sim=100, sort_dag=FALSE)
head(sim_dat)
#>         age    sex proportion
#>       <num> <lgcl>      <num>
#> 1: 52.81824  FALSE  0.1708070
#> 2: 50.86212  FALSE  0.3775658
#> 3: 52.92595   TRUE  0.5807400
#> 4: 51.01464  FALSE  0.4498672
#> 5: 43.14107  FALSE  0.4546994
#> 6: 54.15130   TRUE  0.4961721
```
