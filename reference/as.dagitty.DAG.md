# Transform a `DAG` object into a `dagitty` object

This function extends the `as.dagitty` function from the `dagitty`
package to allow the input of a `DAG` object. The result is a `dagitty`
object that includes only the structure of the DAG, without any
specifications. May be useful to perform identifiability checks etc. on
the `DAG`.

## Usage

``` r
# S3 method for class 'DAG'
as.dagitty(x, include_root_nodes=TRUE,
           include_td_nodes=TRUE, include_networks=FALSE,
           layout=FALSE, ...)
```

## Arguments

- x:

  A `DAG` object created using the
  [`empty_dag`](https://robindenz1.github.io/simDAG/reference/empty_dag.md)
  function with nodes added to it using the `+` syntax. See
  [`?empty_dag`](https://robindenz1.github.io/simDAG/reference/empty_dag.md)
  or [`?node`](https://robindenz1.github.io/simDAG/reference/node.md)
  for more details. Supports DAGs with time-dependent nodes added using
  the [`node_td`](https://robindenz1.github.io/simDAG/reference/node.md)
  function. However, including such DAGs may result in cyclic causal
  structures, because time is not represented in the output matrix.

- include_root_nodes:

  Whether to include root nodes in the output matrix. Should usually be
  kept at `TRUE` (default).

- include_td_nodes:

  Whether to include time-dependent nodes added to the `dag` using the
  [`node_td`](https://robindenz1.github.io/simDAG/reference/node.md)
  function or not. When including these types of nodes, it is possible
  for the adjacency matrix to contain cycles, e.g. that it is not a
  classic DAG anymore, due to the matrix not representing the passage of
  time.

- include_networks:

  Whether to include time-fixed networks added to the `dag` using the
  [`network`](https://robindenz1.github.io/simDAG/reference/network.md)
  function or not. Usually it does not make sense to include those,
  because they are not classical nodes.

- layout:

  Corresponds to the argument of the same name in the
  [`dagitty`](https://rdrr.io/pkg/dagitty/man/dagitty.html) function.

- ...:

  Currently not used.

## Author

Robin Denz

## Value

Returns a `dagitty` object.

## See also

[`empty_dag`](https://robindenz1.github.io/simDAG/reference/empty_dag.md),
[`node`](https://robindenz1.github.io/simDAG/reference/node.md),
[`node_td`](https://robindenz1.github.io/simDAG/reference/node.md),
[`as.igraph.DAG`](https://robindenz1.github.io/simDAG/reference/as.igraph.DAG.md)

## Examples

``` r
library(simDAG)

# some example DAG
dag <- empty_dag() +
  node("death", type="binomial", parents=c("age", "sex"), betas=c(1, 2),
       intercept=-10) +
  node("age", type="rnorm", mean=10, sd=2) +
  node("sex", parents="", type="rbernoulli", p=0.5) +
  node("smoking", parents=c("sex", "age"), type="binomial",
       betas=c(0.6, 0.2), intercept=-2)

if (requireNamespace("dagitty")) {
  g <- dagitty::as.dagitty(dag)
}
```
