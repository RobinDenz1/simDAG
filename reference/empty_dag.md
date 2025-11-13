# Initialize an empty `DAG` object

This function should be used in conjunction with multiple calls to
[`node`](https://robindenz1.github.io/simDAG/reference/node.md) or
[`node_td`](https://robindenz1.github.io/simDAG/reference/node.md) to
create a `DAG` object, which can then be used to simulate data using the
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
and
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
functions.

## Usage

``` r
empty_dag()
```

## Details

Note that this function is only used to initialize an empty `DAG`
object. Actual information about the respective nodes have to be added
using the
[`node`](https://robindenz1.github.io/simDAG/reference/node.md) function
or the
[`node_td`](https://robindenz1.github.io/simDAG/reference/node.md)
function. The documentation page of that function contains more
information on how to correctly do this.

## Value

Returns an empty `DAG` object.

## Author

Robin Denz

## Examples

``` r
library(simDAG)

# just an empty DAG
empty_dag()
#> An empty DAG object without any nodes.

# adding a node to it
empty_dag() + node("age", type="rnorm", mean=20, sd=5)
#> A DAG object with:
#>   -  1  nodes in total
#>   -  1  of which are root nodes
#>   -  0  of which are child nodes
#>   -  0  of which are time-varying nodes
#>   -  0  imposed network structure(s)
```
