# Generate Data from an Aalen Additive Hazards Model

Data from the parents is used to generate the node using Aalen additive
hazards regression using the inversion method. Currently, only
time-constant coefficients and a constant baseline hazard function are
supported.

## Usage

``` r
node_aalen(data, parents, formula=NULL, betas, intercept,
           cens_dist=NULL, cens_args, name,
           as_two_cols=TRUE, left=0)
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
  generated or `NULL` (default). This argument only works if the
  function is used as a node `type` in a
  [`node`](https://robindenz1.github.io/simDAG/reference/node.md) call.
  See [`?node`](https://robindenz1.github.io/simDAG/reference/node.md)
  or the associated vignette for more information about how the
  `formula` argument should be specified in this package.

- betas:

  A numeric vector with length equal to `parents`, specifying the causal
  beta coefficients used to generate the node.

- intercept:

  A single number, specifying the intercept of the model.

- cens_dist:

  A single character naming the distribution function that should be
  used to generate the censoring times or a suitable function. For
  example, `"runif"` could be used to generate uniformly distributed
  censoring times. Set to `NULL` (default) to get no censoring.

- cens_args:

  A list of named arguments which will be passed to the function
  specified by the `cens_dist` argument.

- name:

  A single character string specifying the name of the node.

- as_two_cols:

  Either `TRUE` or `FALSE`, specifying whether the output should be
  divided into two columns. When `cens_dist` is specified, this argument
  will always be treated as `TRUE` because two columns are needed to
  encode both the time to the event and the status indicator. When no
  censoring is applied, however, users may set this argument to `FALSE`
  to simply return the numbers as they are.

- left:

  A single number, specifying the left-truncation time. If set to
  something \> 0, only times that are larger than this value will be
  generated. Is set to 0 by default, so that no left-truncation is used.

## Details

This function generates survival times according to a Aalen additive
hazards model with time-constant beta coefficients and a time-constant
baseline hazard. Time-dependent effects or time-dependent baseline
hazards are currently not supported. To also include censoring, this
function allows the user to supply a function that generates random
censoring times. If the censoring time is smaller than the generated
survival time, the individual is considered censored.

Like the other time-to-event based
[`node`](https://robindenz1.github.io/simDAG/reference/node.md) type
functions, this function usually adds **two** columns to the resulting
dataset instead of one. The first column is called
`paste0(name, "_status")` and is a logical variable, where `TRUE`
indicates that the event has happened and `FALSE` indicates
right-censoring. The second column is named `paste0(name, "_time")` and
includes the survival or censoring time corresponding to the previously
mentioned event indicator. This is the standard format for
right-censored time-to-event data without time-varying covariates. If no
censoring is applied, this behavior can be turned off using the
`as_two_cols` argument.

## References

Aalen, Odd O. A Linear Regression Model for the Analysis of Life Times.
Statistics in Medicine. 1989; (8): 907-925.

## Author

Robin Denz

## Value

Returns a `data.table` of length `nrow(data)` containing two columns if
`as_two_cols=TRUE` and always when `cens_dist` is specified. In this
case, both columns start with the nodes `name` and end with `_status`
and `_time`. The first is a logical vector, the second a numeric one. If
`as_two_cols=FALSE` and `cens_dist` is `NULL`, a numeric vector is
returned instead.

## Examples

``` r
library(simDAG)

set.seed(34543)

# define DAG, here with two baseline covariates and
# no censoring of Y
dag <- empty_dag() +
  node("A", type="runif") +
  node("B", type="rbernoulli") +
  node("Y", type="aalen", formula= ~ 0.1 + A*0.2 + B*-0.05)

sim_dat <- sim_from_dag(dag=dag, n_sim=1000)
head(sim_dat)
#>            A      B     Y_time Y_status
#>        <num> <lgcl>      <num>    <num>
#> 1: 0.8226326  FALSE  0.3544656        1
#> 2: 0.7454597  FALSE  1.9818194        1
#> 3: 0.9127603  FALSE 13.4265135        1
#> 4: 0.7961154   TRUE  7.4812610        1
#> 5: 0.2082226  FALSE  0.9018312        1
#> 6: 0.8906787   TRUE  0.9431440        1
```
