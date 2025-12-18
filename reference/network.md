# Create a network object for a `DAG`

These functions (in conjunction with the
[`empty_dag`](https://robindenz1.github.io/simDAG/reference/empty_dag.md)
and [`node`](https://robindenz1.github.io/simDAG/reference/node.md)
functions) allow users to create `DAG` objects with one or more,
possibly time-varying, network structures linking individual
observations to each other. This makes it possible to simulate data with
complex network-based dependencies among observations using the
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
function or the
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
function.

## Usage

``` r
network(name, net, parents=NULL, ...)

network_td(name, net, parents=NULL, create_at_t0=TRUE, ...)
```

## Arguments

- name:

  A single character string, specifying the name of the network.
  Contrary to the
  [`node`](https://robindenz1.github.io/simDAG/reference/node.md)
  function, multiple values are not allowed, because defining the same
  network multiple times does not make sense.

- net:

  For `network()`, two kinds of inputs are allowed. The first is an
  `igraph` object containing one vertex per observation (e.g. `n_sim`
  vertices) that should be generated when later calling
  [`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
  or
  [`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md).
  The second is a function that generates such an object, given a named
  argument called `n_sim` and any number of further named arguments. For
  `network_td()`, only the latter kind of input is allowed. The vertices
  in the network defined by this variable should not be named. Instead,
  the vertex with index 1 represents row 1 of the generated data, the
  vertex with index 2 represents the second row and so on. Further
  information is given in the details section.

- parents:

  A character vector of names, specifying the parents of the network or
  `NULL` (default). Similar to general nodes, specifying the parents
  allows users to generate a network as a function of the values of the
  `parents`, whenever `net` is a function. If `NULL`, it is assumed that
  the network is generated independently of the data (or already passed
  as `igraph` object). For convenience, it is also allowed to set
  `parents=""` to indicate that the node has no parents.

- create_at_t0:

  Either `TRUE` or `FALSE`, specifying whether the network should be
  generated at time 0 in discrete-time simulations (e.g. when other
  time-independent nodes and networks are generated) or only after the
  creation of data time 0. Defaults to `TRUE`.

- ...:

  Optional further named arguments passed to `net` if it is a function.

## Details

***What does it mean to add a network to a `DAG`?***

When using only
[`node`](https://robindenz1.github.io/simDAG/reference/node.md) or
[`node_td`](https://robindenz1.github.io/simDAG/reference/node.md) to
define a `DAG`, all observations are usually generated independently
from each other (if not explicitly done otherwise using a custom node
function). This reflects the classic i.i.d. assumption that is
frequently used everywhere. For some data generation processes, however,
this assumption is insufficient. The spread of an infectious disease is
a classic example.

The `network()` function allows users to relax this assumption, by
making it possible to define one or more networks that can then be added
to `DAG` objects using the `+` syntax. These networks should contain a
single vertex for each observation that should be generated, placing
each row of the dataset into one place in the network. Through the use
of the [`net`](https://robindenz1.github.io/simDAG/reference/net.md)
function it is then possible to define new nodes as a function of the
neighbors of an observation, where the neighbors of a vertex are defined
as any other vertex that is directly connected to this node. For
example, one could use this capability to use the mean age of an
observations neighbors in a regression model, or use the number of
infected neighbors to model the probability of infection. By combining
this network-simulation approach with the already extensive simulation
capabilities of `DAG` based simulations, almost any DGP can be modelled.
This approach is described more rigorously in the excellent paper given
by Sofrygin et al. (2017).

***Supported network types***:

Users may add any number of networks to a `DAG` object, making it
possible to embed individuals in multiple distinct networks at the same
time. These networks can then be used simultaneously to define a single
or multiple (possibly time-varying) nodes, using multiple
[`net`](https://robindenz1.github.io/simDAG/reference/net.md) function
calls in the respective `formula` arguments. It is also possible to
define time-varying or dynamic networks that change over time, possibly
as a function of the generated data, simulation time or previous states
of the network. Examples are given below and in the associated vignette.

The package directly supports un-directed and directed, un-weighted and
weighted networks. It also supports different definitions of what the
neighbors of an observation are. Note, however, that only networks which
include exactly one vertex per observation are supported.

***Weighted Networks***:

It is possible to supply weighted networks to `network()`. The weights
are then also stored and available to the user when using the
[`net`](https://robindenz1.github.io/simDAG/reference/net.md) function
through the internal `..weight..` variable. For example, if a weighted
network was supplied, the following would be valid syntax:
`net(weighted.mean(A, ..weight..))` (assuming that `A` is a previously
defined variable). Note that the `..weight..` must be used explicitly,
otherwise the weights are ignored.

***Directed Networks***:

Supplying directed networks is also possible. If this is done, users
usually need to specify the `mode` argument of the
[`net`](https://robindenz1.github.io/simDAG/reference/net.md) function
when defining the `formula` arguments. This argument allows users to
define different kinds of neighborhoods for each observation, based on
the direction of the edges.

***Order of Generation***:

Generally, all networks are created in the order in which they were
added to the `DAG`, unless `sort_dag` or `tx_nodes_order` are changed in
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
or
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
respectively. The only exception is that all networks created using the
`network()` function are created *after* all other root nodes have
already been generated.

***Computational considerations***:

Including
[`net()`](https://robindenz1.github.io/simDAG/reference/net.md) terms in
a node might significantly increase the amount of RAM used and the
required computation time, especially with very large networks and / or
large values of `n_sim` and / or `max_t` (the latter is only relevant in
discrete-time simulations using
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)).
The reason for this is that each time a node is generated or updated
over time, the mapping of individuals to their neighbors' values plus
the subsequent aggregation has to be performed, which requires
[`merge()`](https://rdrr.io/r/base/merge.html) calls etc. Usually this
should not be a problem, but it might be for some large discrete-time
simulations. If the same
[`net`](https://robindenz1.github.io/simDAG/reference/net.md) call is
used in multiple nodes it can be beneficial to put it into an extra
[`node`](https://robindenz1.github.io/simDAG/reference/node.md) call and
safe it to avoid re-calculating the same thing over and over again (see
examples).

***Further information***:

For a theoretical treatment, please consult the paper by Sofrygin et al.
(2017), who also describe their slightly different implementation of
this method in the simcausal package. More information on how to specify
network-based dependencies in a `DAG` (using simDAG) after adding a
network, please consult the
[`net`](https://robindenz1.github.io/simDAG/reference/net.md)
documentation page or the associated vignette.

## Value

Returns a `DAG.network` object which can be added to a `DAG` object
directly.

## References

Sofrygin, Oleg, Romain Neugebauer and Mark J. van der Laan (2017).
Conducting Simulations in Causal Inference with Networks-Based
Structural Equation Models. arXiv preprint, doi:
10.48550/arXiv.1705.10376

## Author

Robin Denz

## Examples

``` r
library(igraph)
library(data.table)
library(simDAG)

set.seed(2368)

# generate random undirected / unweighted networks as examples
g1 <- igraph::sample_gnm(n=20, m=30)
g2 <- igraph::sample_gnm(n=20, m=30)

# adding a single network to a DAG, with Y being dependent on
# the mean value of A of its neighbors
dag <- empty_dag() +
  network("Net1", net=g1) +
  node("A", type="rnorm") +
  node("Y", type="gaussian", formula= ~ -2 + net(mean(A))*1.3, error=1.5)

# NOTE: because we supplied the network of size 20 directly, we can only
#       use n_sim=20 here
data <- sim_from_dag(dag, n_sim=20)

# using multiple networks, with Y being differently dependent on
# the mean value of A of its neighbors in both networks
dag <- empty_dag() +
  network("Net1", net=g1) +
  network("Net2", net=g2) +
  node("A", type="rnorm") +
  node("Y", type="gaussian", formula= ~ -2 + net(mean(A), net="Net1")*1.3 +
        net(mean(A), net="Net2")*-2, error=1.5)

# using a function to add networks, to allow any value of 'n_sim' later

# exemplary function that returns a random network of size 'n_sim'
gen_network <- function(n_sim) {
  igraph::sample_gnm(n=n_sim, m=30)
}

# same as first example, but using the function as input
dag <- empty_dag() +
  network("Net1", net=gen_network) +
  node("A", type="rnorm") +
  node("Y", type="gaussian", formula= ~ -2 + net(mean(A))*1.3, error=1.5)
data <- sim_from_dag(dag, n_sim=25)
```
