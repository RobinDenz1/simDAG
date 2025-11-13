# Generate Data from a Cox-Regression Model

Data from the parents is used to generate the node using cox-regression
using the method of Bender et al. (2005).

## Usage

``` r
node_cox(data, parents, formula=NULL, betas, surv_dist, lambda, gamma,
         cens_dist, cens_args, name, as_two_cols=TRUE)
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
  `parents=c("A", "B")` is equal to using `formula= ~ A + B`.

- betas:

  A numeric vector with length equal to `parents`, specifying the causal
  beta coefficients used to generate the node.

- surv_dist:

  A single character specifying the distribution that should be used
  when generating the survival times. Can be either `"weibull"` or
  `"exponential"`.

- lambda:

  A single number used as parameter defined by `surv_dist`.

- gamma:

  A single number used as parameter defined by `surv_dist`.

- cens_dist:

  A single character naming the distribution function that should be
  used to generate the censoring times or a suitable function. For
  example, `"runif"` could be used to generate uniformly distributed
  censoring times. Set to `NULL` to get no censoring.

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

## Details

The survival times are generated according to the cox
proportional-hazards regression model as defined by the user. How
exactly the data-generation works is described in detail in Bender et
al. (2005). To also include censoring, this function allows the user to
supply a function that generates random censoring times. If the
censoring time is smaller than the generated survival time, the
individual is considered censored.

Unlike the other
[`node`](https://robindenz1.github.io/simDAG/reference/node.md) type
functions, this function usually adds **two** columns to the resulting
dataset instead of one. The first column is called
`paste0(name, "_event")` and is a logical variable, where `TRUE`
indicates that the event has happened and `FALSE` indicates
right-censoring. The second column is named `paste0(name, "_time")` and
includes the survival or censoring time corresponding to the previously
mentioned event indicator. This is the standard format for
right-censored time-to-event data without time-varying covariates. If no
censoring is applied, this behavior can be turned off using the
`as_two_cols` argument.

To simulate more complex time-to-event data, the user may need to use
the
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
function instead.

## References

Bender R, Augustin T, Blettner M. Generating survival times to simulate
Cox proportional hazards models. Statistics in Medicine. 2005; 24 (11):
1713-1723.

## Author

Robin Denz

## Value

Returns a `data.table` of length `nrow(data)` containing two columns if
`as_two_cols=TRUE` and always when `cens_dist` is specified. In this
case, both columns start with the nodes `name` and end with `_event` and
`_time`. The first is a logical vector, the second a numeric one. If
`as_two_cols=FALSE` and `cens_dist` is `NULL`, a numeric vector is
returned instead.

## Examples

``` r
library(simDAG)

set.seed(3454)

# define DAG
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("death", type="cox", parents=c("sex", "age"), betas=c(1.1, 0.4),
       surv_dist="weibull", lambda=1.1, gamma=0.7, cens_dist="runif",
       cens_args=list(min=0, max=1))

sim_dat <- sim_from_dag(dag=dag, n_sim=1000)
```
