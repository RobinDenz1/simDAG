# Generate Data from Parametric Survival Models

Data from the parents is used to generate the node using either an
accelerated failure time model, an accelerated hazard model, an extended
hazard model, a proportional odds model or a Young and Prentice model,
as implemented in the rsurv package (Demarqui 2024).

## Usage

``` r
node_aftreg(data, parents, betas, baseline, dist=NULL,
            package=NULL, u=stats::runif(nrow(data)),
            cens_dist=NULL, cens_args, name, as_two_cols=TRUE,
            left=0, right=Inf, ...)

node_ahreg(data, parents, betas, baseline, dist=NULL,
           package=NULL, u=stats::runif(nrow(data)),
           cens_dist=NULL, cens_args, name, as_two_cols=TRUE,
           left=0, right=Inf, ...)

node_ehreg(data, parents, betas, phi, baseline, dist=NULL,
           package=NULL, u=stats::runif(nrow(data)),
           cens_dist=NULL, cens_args, name, as_two_cols=TRUE,
           left=0, right=Inf, ...)

node_poreg(data, parents, betas, baseline, dist=NULL,
           package=NULL, u=stats::runif(nrow(data)),
           cens_dist=NULL, cens_args, name, as_two_cols=TRUE,
           left=0, right=Inf, ...)

node_ypreg(data, parents, betas, phi, baseline, dist=NULL,
           package=NULL, u=stats::runif(nrow(data)),
           cens_dist=NULL, cens_args, name, as_two_cols=TRUE,
           left=0, right=Inf, ...)
```

## Arguments

- data:

  A `data.table` (or something that can be coerced to a `data.table`)
  containing all columns specified by `parents`. Passed to the argument
  of the same name in
  [`raftreg`](https://fndemarqui.github.io/rsurv/reference/raftreg.html),
  [`rahreg`](https://fndemarqui.github.io/rsurv/reference/rahreg.html),
  [`rehreg`](https://fndemarqui.github.io/rsurv/reference/rehreg.html),
  [`rporeg`](https://fndemarqui.github.io/rsurv/reference/rporeg.html)
  or
  [`rypreg`](https://fndemarqui.github.io/rsurv/reference/rypreg.html).

- parents:

  A character vector specifying the names of the parents that this
  particular child node has. Converted into a formula and passed to the
  argument of the same name in
  [`raftreg`](https://fndemarqui.github.io/rsurv/reference/raftreg.html),
  [`rahreg`](https://fndemarqui.github.io/rsurv/reference/rahreg.html),
  [`rehreg`](https://fndemarqui.github.io/rsurv/reference/rehreg.html),
  [`rporeg`](https://fndemarqui.github.io/rsurv/reference/rporeg.html)
  or
  [`rypreg`](https://fndemarqui.github.io/rsurv/reference/rypreg.html).

- betas:

  A numeric vector with length equal to `parents`, specifying the causal
  beta coefficients used to generate the node. Passed to the `beta`
  argument in
  [`raftreg`](https://fndemarqui.github.io/rsurv/reference/raftreg.html),
  [`rahreg`](https://fndemarqui.github.io/rsurv/reference/rahreg.html),
  [`rehreg`](https://fndemarqui.github.io/rsurv/reference/rehreg.html),
  [`rporeg`](https://fndemarqui.github.io/rsurv/reference/rporeg.html)
  or
  [`rypreg`](https://fndemarqui.github.io/rsurv/reference/rypreg.html).

- phi:

  A numeric vector with length equal to `parents`, specifying the phi
  beta coefficients used to generate the node. Only required for
  extended hazard and Yang and Prentice models. Passed to the `phi`
  argument in
  [`rehreg`](https://fndemarqui.github.io/rsurv/reference/rehreg.html)
  or
  [`rypreg`](https://fndemarqui.github.io/rsurv/reference/rypreg.html).

- baseline:

  A single character string, specifying the name of the baseline
  survival distribution. Passed to the argument of the same name in
  [`raftreg`](https://fndemarqui.github.io/rsurv/reference/raftreg.html),
  [`rahreg`](https://fndemarqui.github.io/rsurv/reference/rahreg.html),
  [`rehreg`](https://fndemarqui.github.io/rsurv/reference/rehreg.html),
  [`rporeg`](https://fndemarqui.github.io/rsurv/reference/rporeg.html)
  or
  [`rypreg`](https://fndemarqui.github.io/rsurv/reference/rypreg.html).

- dist:

  An alternative way to specify the baseline survival distribution.
  Passed to the argument of the same name in
  [`raftreg`](https://fndemarqui.github.io/rsurv/reference/raftreg.html),
  [`rahreg`](https://fndemarqui.github.io/rsurv/reference/rahreg.html),
  [`rehreg`](https://fndemarqui.github.io/rsurv/reference/rehreg.html),
  [`rporeg`](https://fndemarqui.github.io/rsurv/reference/rporeg.html)
  or
  [`rypreg`](https://fndemarqui.github.io/rsurv/reference/rypreg.html).

- package:

  A single character string, specifying the name of the package where
  the assumed quantile function is implemented. Passed to the argument
  of the same name in
  [`raftreg`](https://fndemarqui.github.io/rsurv/reference/raftreg.html),
  [`rahreg`](https://fndemarqui.github.io/rsurv/reference/rahreg.html),
  [`rehreg`](https://fndemarqui.github.io/rsurv/reference/rehreg.html),
  [`rporeg`](https://fndemarqui.github.io/rsurv/reference/rporeg.html)
  or
  [`rypreg`](https://fndemarqui.github.io/rsurv/reference/rypreg.html).

- u:

  A numeric vector of quantiles of length `nrow(data)`. Usually this
  should simply be passed a vector of randomly generated uniformly
  distributed values between 0 and 1, as defined by the default. Passed
  to the argument of the same name in
  [`raftreg`](https://fndemarqui.github.io/rsurv/reference/raftreg.html),
  [`rahreg`](https://fndemarqui.github.io/rsurv/reference/rahreg.html),
  [`rehreg`](https://fndemarqui.github.io/rsurv/reference/rehreg.html),
  [`rporeg`](https://fndemarqui.github.io/rsurv/reference/rporeg.html)
  or
  [`rypreg`](https://fndemarqui.github.io/rsurv/reference/rypreg.html).

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

- left:

  Either a single number `>= 0`, or a numeric vector of length
  `nrow(data)` containing only numbers `>= 0`. When simulating the
  survival times, only numbers larger than these will be generated using
  correctly left-truncated sampling. Note that this does not affect the
  censoring times, only the generated survival times will be
  left-truncated. Set to 0 (default) to not use left-truncation.
  Corresponds to the `lwr` argument in the underlying rsurv functions.
  Only works with `rsurv` package version \>= 0.0.3 (currently only
  available on github).

- right:

  Same as the `left` argument, but for right-truncation, meaning that no
  values greater than `right` will be generated. Corresponds to the
  `upr` argument in the underlying rsurv functions. Only works with
  `rsurv` package version \>= 0.0.3 (currently only available on
  github).

- ...:

  Further arguments passed to
  [`raftreg`](https://fndemarqui.github.io/rsurv/reference/raftreg.html),
  [`rahreg`](https://fndemarqui.github.io/rsurv/reference/rahreg.html),
  [`rehreg`](https://fndemarqui.github.io/rsurv/reference/rehreg.html),
  [`rporeg`](https://fndemarqui.github.io/rsurv/reference/rporeg.html)
  or
  [`rypreg`](https://fndemarqui.github.io/rsurv/reference/rypreg.html).

## Details

Survival times are generated according to the specified parametric
survival model. The actual generation of the values is done entirely by
calls to the rsurv package. All arguments are directly passed to the
corresponding function in rsurv. Only the censoring is added on top of
it. These convenience wrappers only exist to allow direct integration of
these data generation functions with the interface provided by simDAG.
Please consult the documentation and excellent paper by Demarqui (2024)
for more information about the models and how to specify the arguments.

## References

Demarqui Fabio N. Simulation of Survival Data with the Package rsurv.
(2024) arXiv:2406.01750v1.

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

if (requireNamespace("rsurv")) {

library(rsurv)

# accelerated failure time model
dag <- empty_dag() +
  node(c("A", "B", "C"), type="rnorm", mean=0, sd=1) +
  node("Y", type="aftreg", formula= ~ -2 + A*0.2 + B*0.1 + A:B*1,
       baseline="weibull", shape=1, scale=2)
data <- sim_from_dag(dag, n_sim=100)

# accelerated hazard model
dag <- empty_dag() +
  node(c("A", "B", "C"), type="rnorm", mean=0, sd=1) +
  node("Y", type="ahreg", formula= ~ -2 + A*0.2 + B*0.1,
       baseline="weibull", shape=1, scale=2)
data <- sim_from_dag(dag, n_sim=100)

# extended hazard model
dag <- empty_dag() +
  node(c("A", "B", "C"), type="rnorm", mean=0, sd=1) +
  node("Y", type="ehreg", formula= ~ -2 + A*0.2 + B*0.1,
       baseline="weibull", shape=1, scale=2,
       phi=c(-1, 1))
data <- sim_from_dag(dag, n_sim=100)

# proportional odds model
dag <- empty_dag() +
  node(c("A", "B", "C"), type="rnorm", mean=0, sd=1) +
  node("Y", type="poreg", formula= ~ -2 + A*0.2 + B*0.1,
       baseline="weibull", shape=1, scale=2)
data <- sim_from_dag(dag, n_sim=100)

# Young and Prentice model
dag <- empty_dag() +
  node(c("A", "B", "C"), type="rnorm", mean=0, sd=1) +
  node("Y", type="ypreg", formula= ~ -2 + A*0.2 + B*0.1,
       baseline="weibull", shape=1, scale=2,
       phi=c(-1, 1))
data <- sim_from_dag(dag, n_sim=100)
}
#> Loading required namespace: rsurv
```
