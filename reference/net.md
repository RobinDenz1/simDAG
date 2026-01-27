# Specify Network Dependencies in a `DAG`

This function may be used in the `formula` of nodes in which the value
of the observation of one individual are dependent on its' neighbors in
a defined static
[`network`](https://robindenz1.github.io/simDAG/reference/network.md) or
dynamic
[`network_td`](https://robindenz1.github.io/simDAG/reference/network.md).
Given the network and a previously generated variable, `net()`
aggregates data of the neighbors according to an arbitrary function
under the hood. The resulting variable can then be used directly in a
`formula`.

## Usage

``` r
net(expr, net=NULL, mode="all", order=1,
    mindist=0, na=NA)
```

## Arguments

- expr:

  Any R expression, usually containing one or more previously generated
  variables, that returns one numeric value given a vector, such as
  `sum(variable)` or `mean(variable)`.

- net:

  A single character string specifying the name of the network that
  should be used to define the neighbors of an observation. If only one
  network is present in the `DAG`, this argument can be omitted. The
  single added network is then used by default. If multiple networks are
  present and this argument is not defined, an error will be produced.

- mode:

  A single character, specifying how to use the direction of the edges
  if a directed network is supplied (ignored otherwise). If `"all"`, the
  direction of the edges is ignored and both incoming and outgoing edges
  are used to define the neighbors of each individual. If `"out"`, only
  the individuals who \\i\\ (the observation row) is pointing to are
  used as neighbors and if `"in"` only the individuals who point to
  \\i\\ are being used as neighbors.

- order:

  A single integer giving the order of the neighborhood. If `order=1`
  (default), only the vertices that are directly connected to vertex
  \\i\\ are considered its neighbors. If `order=2`, all vertices
  connected to those neighbors are also considered the neighbors of
  vertex \\i\\ and so on.

- mindist:

  A single integer \>= 0, specifying the minimum distance the neighbors
  needs to have to an observation to be considered neighbors. Only makes
  sense with `order > 1`.

- na:

  A single value assigned to the variable if `expr` could not be
  computed. This can happen due to the nature of the expression (e.g.
  `NA` being returned directly after evaluating the expression for some
  reason), or when an observation does not have any neighbors in a
  network.

## Details

***How it works***:

Internally the following procedure is used whenever a `net()` function
call is included in a `formula` of a
[`node`](https://robindenz1.github.io/simDAG/reference/node.md)
(regardless of whether time-fixed or time-dependent). First, the
associated network (defined using the `net` argument) is used to
identify the neighbors of each observation. Every vertex that is
directly connected to an observation is considered its' neighbor. The
parent variable(s) specified in the `net()` call are then aggregated
over these neighbors using the given `expr`. A simple example: consider
observation `1` with four neighbors named `2, 5, 8` and `10`. The
`formula` contains the following `net()` call: `net(sum(infected))`. The
value of the `infected` variable is 0, 0, 1, 1 for persons `2, 5, 8` and
`10` respectively. These values are then summed up to result in a value
of 2 for person `1`. The same is done for every person in the simulated
data. The resulting variable is then used as-is in the simulation.

***Supported inputs***:

Any function that returns a single (usually numeric) value, given the
neighbors' values can be used. It is therefore also possible to make the
simulation dependent on specific neighbors only. For example, using
`infected[1]` instead of `sum(infected)` would return a value of 0 for
observation 1 in the above example, because person `2` is the first
neighbor and has a value of 0. Note that the internally used variable
named `..neighbor..` includes the ids of the neighbors. The entire
`expr` is evaluated in a data.table call of the form:
`data[, .(variable = eval(expr)), by=id]`, making it also possible to
use any data.table syntax such as
[`.N`](https://rdrr.io/pkg/data.table/man/special-symbols.html) (which
would return the number of neighbors a person has).

***Specifying parents***:

Whenever a `net()` call is used in a `formula`, we recommend specifying
the `parents` argument of the node as well. The reason for this
recommendation is, that it is sometimes difficult to identify which
variables are used in `net()` calls, depending on the `expr`. This may
cause issues if a `DAG` is not specified in a topologically sorted
manner and users rely on the `sort_dag` argument of
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
to re-order the variables. Specifying the `parents` ensures that this
issue cannot occur.

***A small warning***:

Note that it never really makes sense to use this function outside of a
`formula` argument: if you look at its source code you will realize that
it does not actually do anything, except returning its input. It is only
a piece of syntax for the `formula` interface. Please consult the
[`network`](https://robindenz1.github.io/simDAG/reference/network.md)
documentation page or the associated vignette for more information.

## Value

"Returns" a vector of length `n_sim` when used properly in a
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
or
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
call. Returns a list of its input when used outside `formula`.

## Author

Robin Denz

## Examples

``` r
library(igraph)
#> 
#> Attaching package: ‘igraph’
#> The following objects are masked from ‘package:stats’:
#> 
#>     decompose, spectrum
#> The following object is masked from ‘package:base’:
#> 
#>     union
library(data.table)
library(simDAG)

# define a random network for illustration, with 10 vertices
set.seed(234)
g <- igraph::sample_smallworld(1, 10, 2, 0.5)

# a simple dag containing only two time-constant variables and the network
dag <- empty_dag() +
  node("A", type="rnorm", mean=0, sd=1) +
  node("B", type="rbernoulli", p=0.5) +
  network("Net1", net=g)

# using the mean of A of each observations neighbor in a linear model
dag2 <- dag +
  node("Y", type="gaussian", formula= ~ -2 + net(mean(A))*4, error=1)

# using an indicator of whether any of an observations neighbors has
# a 1 in B in a linear model
dag3 <- dag +
  node("Y", type="gaussian", formula= ~ 1.5 + net(as.numeric(any(B==1)))*3,
       error=1.2)
```
