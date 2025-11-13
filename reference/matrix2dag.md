# Obtain a `DAG` object from a Adjacency Matrix and a List of Node Types

The
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
function requires the user to specify the causal relationships inside a
`DAG` object containing node information. This function creates such an
object using a adjacency matrix and a list of node types. The resulting
`DAG` will be only partially specified, which may be useful for the
[`dag_from_data`](https://robindenz1.github.io/simDAG/reference/dag_from_data.md)
function.

## Usage

``` r
matrix2dag(mat, type)
```

## Arguments

- mat:

  A p x p adjacency matrix where p is the number of variables. The
  matrix should be filled with zeros. Only places where the variable
  specified by the row has a direct causal effect on the variable
  specified by the column should be 1. Both the columns and the rows
  should be named with the corresponding variable names.

- type:

  A named list with one entry for each variable in `mat`, specifying the
  `type` of the corresponding node. See
  [`node`](https://robindenz1.github.io/simDAG/reference/node.md) for
  available node types.

## Details

An adjacency matrix is simply a square matrix in which each node has one
column and one row associated with it. For example, if the node A has a
causal effect on node B, the matrix will contain `1` in the spot
`matrix["A", "B"]`. This function uses this kind of matrix and
additional information about the node type to create a `DAG` object. The
resulting `DAG` cannot be used in the
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
function directly, because it will not contain the necessary parameters
such as beta-coefficients or intercepts etc. It may, however, be passed
directly to the
[`dag_from_data`](https://robindenz1.github.io/simDAG/reference/dag_from_data.md)
function. This is pretty much it's only valid use-case. If the goal is
to to specify a full `DAG` manually, the user should use the
[`empty_dag`](https://robindenz1.github.io/simDAG/reference/empty_dag.md)
function in conjunction with
[`node`](https://robindenz1.github.io/simDAG/reference/node.md) calls
instead, as described in the respective documentation pages and the
vignettes.

The output will never contain time-dependent nodes. If this is
necessary, the user needs to manually define the DAG.

## Author

Robin Denz

## Value

Returns a partially specified `DAG` object.

## See also

[`empty_dag`](https://robindenz1.github.io/simDAG/reference/empty_dag.md),
[`node`](https://robindenz1.github.io/simDAG/reference/node.md),
[`node_td`](https://robindenz1.github.io/simDAG/reference/node.md),
[`dag_from_data`](https://robindenz1.github.io/simDAG/reference/dag_from_data.md)

## Examples

``` r
library(simDAG)

# simple example adjacency matrix
mat <- matrix(c(0, 0, 1, 0, 0, 1, 0, 0, 0), ncol=3, byrow=TRUE)
colnames(mat) <- c("age", "sex", "death")
rownames(mat) <- c("age", "sex", "death")

type <- list(age="rnorm", sex="rbernoulli", death="binomial")

matrix2dag(mat=mat, type=type)
#> A DAG object with:
#>   -  3  nodes in total
#>   -  2  of which are root nodes
#>   -  1  of which are child nodes
#>   -  0  of which are time-varying nodes
#>   -  0  imposed network structure(s)
```
