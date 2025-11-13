# Generate Data based on an expression

This node type may be used to generate a new node given a regular R
expression that may include function calls or any other valid R syntax.
This may be useful to combine components of a node which need to be
simulated with separate
[`node`](https://robindenz1.github.io/simDAG/reference/node.md) calls,
or just as a convenient shorthand for some variable transformations.
Also allows calculation of just the linear predictor and generation of
intermediary variables using the enhanced `formula` syntax.

## Usage

``` r
node_identity(data, parents, formula, kind="expr",
              betas, intercept, var_names=NULL,
              name=NULL, dag=NULL)
```

## Arguments

- data:

  A `data.table` (or something that can be coerced to a `data.table`)
  containing all columns specified by `parents`.

- parents:

  A character vector specifying the names of the parents that this
  particular child node has. When using this function as a node type in
  [`node`](https://robindenz1.github.io/simDAG/reference/node.md) or
  [`node_td`](https://robindenz1.github.io/simDAG/reference/node.md),
  this argument usually does not need to be specified because the
  `formula` argument is required and contains all needed information
  already.

- formula:

  A `formula` object. The specific way this argument should be specified
  depends on the value of the `kind` argument used. It can be an
  expression (`kind="expr"`), a `simDAG` style enhanced formula to
  calculate the linear predictor only (`kind="linpred"`) or used as a
  way to store intermediary variable transformations (`kind="data"`).

- kind:

  A single character string specifying how the `formula` should be
  interpreted, with three allowed values: `"expr"`, `"linpred"` and
  `"data"`. If `"expr"` (default), the formula should contain a `~`
  symbol with nothing on the LHS, and any valid R expression that can be
  evaluated on `data` on the RHS. This expression needs to contain at
  least one variable name (otherwise users may simply use
  [`rconstant`](https://robindenz1.github.io/simDAG/reference/rconstant.md)
  as node type). It may contain any number of function calls or other
  valid R syntax, given that all contained objects are included in the
  global environment. Note that the usual `formula` syntax, using for
  example `A:B*0.2` to specify an interaction won't work in this case.
  If that is the goal, users should use `kind="linpred"`, in which case
  the `formula` is interpreted in the normal `simDAG` way and the linear
  combination of the variables is calculated. Finally, if `kind="data"`,
  the `formula` may contain any enhanced `formula` syntax, such as `A:B`
  or [`net()`](https://robindenz1.github.io/simDAG/reference/net.md)
  calls, but it should not contain beta-coefficients or an `intercept`.
  In this case, the transformed variables are returned in the order
  given, using the `name` as column names. See examples.

- betas:

  Only used internally when `kind="linpred"`.

- intercept:

  Only used internally when `kind="linpred"`. If no intercept should be
  present, it should still be added to the formula using a simple 0, for
  example `~ 0 + A*0.2 + B*0.3`

- var_names:

  Only used when `kind="data"`. In this case, and only if there are
  multiple terms on the right-hand side of `formula`, the resulting
  columns will be re-named according to this argument. Should have the
  same length as the number of terms in `formula`. Names are given in
  the same order as the variables appear in `formula`. If only a single
  term is on the right-hand side of `formula`, the `name` supplied in
  the [`node`](https://robindenz1.github.io/simDAG/reference/node.md)
  function call will automatically be used as the nodes name and this
  argument is ignored. Set to `NULL` (default) to just use the terms as
  names.

- name:

  A single character string, specifying the name of the node. Passed
  internally only. See `var_names`.

- dag:

  The `dag` that this node is a part of. Will be passed internally if
  needed (for example when performing networks-based simulations). This
  argument can therefore always be ignored by users.

## Details

When using `kind="expr"`, custom functions and objects can be used
without issues in the `formula`, but they need to be present in the
global environment, otherwise the underlying
[`eval()`](https://rdrr.io/r/base/eval.html) function call will fail.
Using this function outside of
[`node`](https://robindenz1.github.io/simDAG/reference/node.md) or
[`node_td`](https://robindenz1.github.io/simDAG/reference/node.md) is
essentially equal to using `with(data, eval(formula))` (without the `~`
in the `formula`). If `kind!="expr"`, this function cannot be used
outside of a defined `DAG`.

Please note that when using identity nodes with `kind="data"` and
multiple terms in `formula`, the printed structural equations and plots
of a `dag` object may not be correct.

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

set.seed(12455432)

#### using kind = "expr" ####

# define a DAG
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("something", type="identity", formula= ~ age + sex + 2)

sim_dat <- sim_from_dag(dag=dag, n_sim=100)
head(sim_dat)
#>         age    sex something
#>       <num> <lgcl>     <num>
#> 1: 46.90669  FALSE  48.90669
#> 2: 47.25599  FALSE  49.25599
#> 3: 47.05490  FALSE  49.05490
#> 4: 48.06434  FALSE  50.06434
#> 5: 51.62119  FALSE  53.62119
#> 6: 54.11589  FALSE  56.11589

# more complex alternative
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("something", type="identity",
       formula= ~ age / 2 + age^2 - ifelse(sex, 2, 3) + 2)

sim_dat <- sim_from_dag(dag=dag, n_sim=100)
head(sim_dat)
#>         age    sex something
#>       <num> <lgcl>     <num>
#> 1: 48.75325   TRUE  2401.256
#> 2: 48.62980   TRUE  2389.173
#> 3: 56.19418   TRUE  3185.883
#> 4: 53.39997  FALSE  2877.257
#> 5: 45.73172   TRUE  2114.256
#> 6: 56.06411  FALSE  3170.217

#### using kind = "linpred" ####

# this would work with both kind="expr" and kind="linpred"
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("pred", type="identity", formula= ~ 1 + age*0.2 + sex*1.2,
       kind="linpred")

sim_dat <- sim_from_dag(dag=dag, n_sim=10)
head(sim_dat)
#>         age    sex     pred
#>       <num> <lgcl>    <num>
#> 1: 52.92694   TRUE 12.78539
#> 2: 50.39227  FALSE 11.07845
#> 3: 56.38631  FALSE 12.27726
#> 4: 53.92836   TRUE 12.98567
#> 5: 55.23475  FALSE 12.04695
#> 6: 49.03310  FALSE 10.80662

# this only works with kind="linpred", due to the presence of a special term
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5, output="numeric") +
  node("pred", type="identity", formula= ~ 1 + age*0.2 + sex*1.2 + age:sex*-2,
       kind="linpred")

sim_dat <- sim_from_dag(dag=dag, n_sim=10)
head(sim_dat)
#>         age   sex       pred
#>       <num> <num>      <num>
#> 1: 52.90500     0  11.581000
#> 2: 44.52789     0   9.905577
#> 3: 47.58274     1 -83.448929
#> 4: 52.43168     1 -92.177030
#> 5: 49.61593     0  10.923186
#> 6: 51.83440     1 -91.101923

#### using kind = "data" ####

# simply return the transformed data, useful if the terms are used
# frequently in multiple nodes in the DAG to save computation time

# using only a single interaction term
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5, output="numeric") +
  node("age_sex_interact", type="identity", formula= ~ age:sex, kind="data")

sim_dat <- sim_from_dag(dag=dag, n_sim=10)
head(sim_dat)
#>         age   sex age_sex_interact
#>       <num> <num>            <num>
#> 1: 50.61902     0          0.00000
#> 2: 51.73089     0          0.00000
#> 3: 50.12556     1         50.12556
#> 4: 48.68929     0          0.00000
#> 5: 43.91665     0          0.00000
#> 6: 49.24920     0          0.00000

# using multiple terms
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5, output="numeric") +
  node("name_not_used", type="identity", formula= ~ age:sex + I(age^2),
       kind="data", var_names=c("age_sex_interact", "age_squared"))

sim_dat <- sim_from_dag(dag=dag, n_sim=10)
head(sim_dat)
#>         age   sex age_sex_interact age_squared
#>       <num> <num>            <num>       <num>
#> 1: 44.07303     0          0.00000    1942.432
#> 2: 50.21162     0          0.00000    2521.207
#> 3: 51.79592     1         51.79592    2682.817
#> 4: 48.30182     0          0.00000    2333.066
#> 5: 55.40266     0          0.00000    3069.454
#> 6: 43.40470     1         43.40470    1883.968
```
