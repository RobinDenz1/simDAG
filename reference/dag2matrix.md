# Obtain a Adjacency Matrix from a `DAG` object

The
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
function requires the user to specify the causal relationships inside a
`DAG` object containing node information. This function takes this
object as input and outputs the underlying adjacency matrix. This can be
useful to plot the theoretical DAG or to check if the nodes have been
specified correctly.

## Usage

``` r
dag2matrix(dag, include_root_nodes=TRUE, include_td_nodes=FALSE,
           include_networks=FALSE)
```

## Arguments

- dag:

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
  because they are not classical nodes. This is mostly used internally
  to ensure that the generation of nodes and networks is processed in
  the right order.

## Details

An adjacency matrix is simply a square matrix in which each node has one
column and one row associated with it. For example, if the node A has a
causal effect on node B, the matrix will contain `1` in the spot
`matrix["A", "B"]`.

If a time-varying node is also defined as a time-fixed node, the parents
of both parts will be pooled when creating the output matrix.

## Author

Robin Denz

## Value

Returns a numeric square matrix with one row and one column per used
node in `dag`.

## See also

[`empty_dag`](https://robindenz1.github.io/simDAG/reference/empty_dag.md),
[`node`](https://robindenz1.github.io/simDAG/reference/node.md),
[`node_td`](https://robindenz1.github.io/simDAG/reference/node.md)

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

# get adjacency matrix
dag2matrix(dag)
#>         age sex death smoking
#> age       0   0     1       1
#> sex       0   0     1       1
#> death     0   0     0       0
#> smoking   0   0     0       0

# get adjacency matrix using only the child nodes
dag2matrix(dag, include_root_nodes=FALSE)
#>         death smoking
#> death       0       0
#> smoking     0       0

## adding time-varying nodes
dag <- dag +
  node_td("disease", type="time_to_event", parents=c("age", "smoking"),
          prob_fun=0.01) +
  node_td("cve", type="time_to_event", parents=c("age", "sex", "smoking",
                                                 "disease"),
          prob_fun=0.001, event_duration=Inf)

# get adjacency matrix including all nodes
dag2matrix(dag, include_td_nodes=TRUE)
#>         age sex death smoking disease cve
#> age       0   0     1       1       1   1
#> sex       0   0     1       1       0   1
#> death     0   0     0       0       0   0
#> smoking   0   0     0       0       1   1
#> disease   0   0     0       0       0   1
#> cve       0   0     0       0       0   0

# get adjacency matrix including only time-constant nodes
dag2matrix(dag, include_td_nodes=FALSE)
#>         age sex death smoking
#> age       0   0     1       1
#> sex       0   0     1       1
#> death     0   0     0       0
#> smoking   0   0     0       0

# get adjacency matrix using only the child nodes
dag2matrix(dag, include_root_nodes=FALSE)
#>         death smoking
#> death       0       0
#> smoking     0       0
```
