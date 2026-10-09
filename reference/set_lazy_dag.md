# Create a fully defined `DAG` object from a `lazy_DAG` object

Given a un-evaluated `lazy_DAG` object, created using the
[`lazy_dag`](https://robindenz1.github.io/simDAG/reference/lazy_dag.md)
function and a named list or vector of parameters, this function returns
a fully specified `DAG` object that can then be used to run actual
simulations using
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md),
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
or
[`sim_discrete_event`](https://robindenz1.github.io/simDAG/reference/sim_discrete_event.md).
See details and
[`?lazy_dag`](https://robindenz1.github.io/simDAG/reference/lazy_dag.md)
for more information.

## Usage

``` r
set_lazy_dag(lazy_dag, params)
```

## Arguments

- lazy_dag:

  A `lazy_DAG` object created using the
  [`lazy_dag`](https://robindenz1.github.io/simDAG/reference/lazy_dag.md)
  function. See
  [`?lazy_dag`](https://robindenz1.github.io/simDAG/reference/lazy_dag.md)
  for more information on how to specify a lazy DAG. See
  [`?node`](https://robindenz1.github.io/simDAG/reference/node.md) for
  more information on how to specify a regular `DAG`.

- params:

  A named list or a named vector, containing one entry per `param()`
  call used in the creation of the `lazy_dag` object. These entries
  should be scalar values that can be subsituted directly into the DAG
  definition.

## Details

Given the previously un-evaluated `lazy_DAG` object, this function
substitutes all `param()` pieces with the corresponding parameter
defined in `params` and then finally evaluates the `DAG`. Note that the
`lazy_DAG` object itself is not changed in place, so that the
`set_lazy_dag()` function may be used multiple times on the same
`lazy_DAG` to create multiple different data generation processes.

## Value

Returns a `DAG` object.

## Author

Robin Denz

## Examples

``` r
library(simDAG)

# define a lazy DAG with one unspecified parameter
ldag <- lazy_dag(
  empty_dag() +
    node("A", type="rnorm", mean=1, sd=param(SD))
)

# get a real DAG object with SD = 1 and another with SD = 2
dag1 <- set_lazy_dag(ldag, params=c(SD=1))
dag2 <- set_lazy_dag(ldag, params=c(SD=2))

# define some lazy DAG, leaving the intercept of "death" and one beta
# coefficient of "smoking" unspecified
ldag <- lazy_dag(
  empty_dag() +
    node("age", type="rnorm", mean=10, sd=2) +
    node("sex", parents="", type="rbernoulli", p=0.5) +
    node("smoking", type="binomial",
         formula= ~ -2 + age*param(Sbeta) + sex*0.2) +
    node("death", type="binomial",
         formula= ~ param(Dintercept) + age*1 + sex*2)
)

# get a real DAG object from this with set values
dag3 <- set_lazy_dag(ldag, params=c(Sbeta=2.1, Dintercept=-10))
```
