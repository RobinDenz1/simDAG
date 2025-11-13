# Add a `DAG.node` or a `DAG.network` object to a `DAG` object

This function allows users to add `DAG.node` objects created using the
[`node`](https://robindenz1.github.io/simDAG/reference/node.md) or
[`node_td`](https://robindenz1.github.io/simDAG/reference/node.md)
function and `DAG.network` objects created using the
[`network`](https://robindenz1.github.io/simDAG/reference/network.md) or
[`network_td`](https://robindenz1.github.io/simDAG/reference/network.md)
function to `DAG` objects created using the
[`empty_dag`](https://robindenz1.github.io/simDAG/reference/empty_dag.md)
function, which makes it easy to fully specify a DAG to use in the
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
function and
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md).

## Usage

``` r
add_node(dag, node)

# S3 method for class 'DAG'
object_1 + object_2
```

## Arguments

- dag:

  A `DAG` object created using the
  [`empty_dag`](https://robindenz1.github.io/simDAG/reference/empty_dag.md)
  function.

- node:

  Either a `DAG.node` object created using the
  [`node`](https://robindenz1.github.io/simDAG/reference/node.md)
  function or
  [`node_td`](https://robindenz1.github.io/simDAG/reference/node.md)
  function, or a `DAG.network` object created using the
  [`network`](https://robindenz1.github.io/simDAG/reference/network.md)
  function or
  [`network_td`](https://robindenz1.github.io/simDAG/reference/network.md)
  function.

- object_1:

  Either a `DAG` object, a `DAG.node` object or a `DAG.network` object.
  The order of the objects does not change the result.

- object_2:

  See argument `object_1`.

## Details

The two ways of adding a node or a network to a `DAG` object are:
`dag <- add_node(dag, node(...))` and `dag <- dag + node(...)`, which
give identical results (note that the `...` should be replaced with
actual arguments and that the initial `dag` should be created with a
call to `empty_dag`). See
[`node`](https://robindenz1.github.io/simDAG/reference/node.md) for more
information on how to specify a `DAG` for use in the
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
and [`node_td`](https://robindenz1.github.io/simDAG/reference/node.md)
functions.

## Value

Returns an `DAG` object with the `DAG.node` object or `DAG.network`
object added to it.

## Author

Robin Denz

## Examples

``` r
library(simDAG)

## add nodes to DAG using +
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=5) +
  node("sex", type="rbernoulli", p=0.5) +
  node("income", type="gaussian", parents=c("age", "sex"), betas=c(1.1, 0.2),
       intercept=-5, error=4)

## add nodes to DAG using add_node()
dag <- empty_dag()
dag <- add_node(dag, node("age", type="rnorm", mean=50, sd=5))
```
