# Define a partial `DAG` object without evaluating it

This function should be used as a wrapper around an expression that aims
to generate a `DAG` object, but with some parameters left un-specified.
Those parameters should be defined using the `param()` syntax, as shown
below. The `DAG` is then not instantly evaluated. Instead, it will only
be evaluated later using a
[`set_lazy_dag`](https://robindenz1.github.io/simDAG/reference/set_lazy_dag.md)
call, which then sets the still un-specified parameters. This may be
useful to create flexible functions that generate `DAG` objects during
simulations, and is also used in `calibrate_dag`.

## Usage

``` r
lazy_dag(expr)
```

## Arguments

- expr:

  An expression that would usually create a `DAG` object. This should
  include one
  [`empty_dag`](https://robindenz1.github.io/simDAG/reference/empty_dag.md)
  call and one or more added
  [`node`](https://robindenz1.github.io/simDAG/reference/node.md) or
  [`node_td`](https://robindenz1.github.io/simDAG/reference/node.md)
  functions. Importantly, at least one parameter of one node should be
  left unspecified, using the `param()` syntax. See
  [`?node`](https://robindenz1.github.io/simDAG/reference/node.md) for
  more information on how to specify a regular DAG. See details on how
  to use the `param()` syntax.

## Details

It is sometimes convenient to define a `DAG` object, but to leave some
parameters of it flexible. For example, if we wanted to conduct a
Monte-Carlo simulation study with different values of an interaction
coefficient in a `formula` of a
[`node`](https://robindenz1.github.io/simDAG/reference/node.md), we need
different `DAG` definitions for each scenario. Instead of copying and
pasting nearly the same code multiple times, this function allows users
to define a single partially specified `DAG` that leaves one or multiple
parameters of a `DAG` definition unspecified.

This works the same way a usual `DAG` construction does. The only two
differences are (1) that the whole expression (`expr`) that creates the
`DAG` should be wrapped in a `lazy_dag()` function call and (2) that
parameters that should remain flexible are wrapped in a `param()`
function. For example, if we wish to leave the intercept in a `formula`
flexible, we could use `param(intercept)`. There is no limit on the
amount of flexible parameters that can be specified. They also do not
have to be in a `formula`, but can be passed to any argument of any
`node` call. The only requirements are that the names used inside the
`param()` call are unique, so that they can later be unambiguously
identified.

Once a `lazy_DAG` is created, users may create real `DAG` objects from
it using the
[`set_lazy_dag`](https://robindenz1.github.io/simDAG/reference/set_lazy_dag.md)
function, which only requires the `lazy_DAG` and a vector of `params` as
input. See
[`?set_lazy_dag`](https://robindenz1.github.io/simDAG/reference/set_lazy_dag.md)
for more information. Before running
[`set_lazy_dag()`](https://robindenz1.github.io/simDAG/reference/set_lazy_dag.md),
it is not possible to use these objects to simulate any data, because
they are not fully specified or even executed yet. Note also that
`param()` is just a piece of syntax. The `param()` function itself does
not actually exist in the simDAG package.

## Value

Returns a `lazy_DAG` object.

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
