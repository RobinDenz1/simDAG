# Simulate Data from a `DAG`

This function can be used to generate data from a given `DAG`. The `DAG`
should be created using the
[`empty_dag`](https://robindenz1.github.io/simDAG/reference/empty_dag.md)
and [`node`](https://robindenz1.github.io/simDAG/reference/node.md)
functions, which require the user to fully specify all variables,
including information about distributions, beta coefficients and,
depending on the node type, more parameters such as intercepts. Network
dependencies among observations may also be included using the
[`network`](https://robindenz1.github.io/simDAG/reference/network.md)
function.

## Usage

``` r
sim_from_dag(dag, n_sim, sort_dag=FALSE, return_networks=FALSE,
             check_inputs=TRUE)
```

## Arguments

- dag:

  A `DAG` object created using the
  [`empty_dag`](https://robindenz1.github.io/simDAG/reference/empty_dag.md)
  function with
  [`node`](https://robindenz1.github.io/simDAG/reference/node.md) calls
  (and potentially
  [`network`](https://robindenz1.github.io/simDAG/reference/network.md)
  calls) added to it using the `+` syntax. See details.

- n_sim:

  A single number specifying how many observations should be generated.

- sort_dag:

  Whether to topologically sort the DAG before starting the simulation
  or not. If the nodes in `dag` were already added in a topologically
  sorted manner, this argument can be kept at `FALSE`. It is recommended
  to not rely on this argument too heavily, because sorting may
  sometimes fail when only a `formula` is supplied to one or more
  [`node`](https://robindenz1.github.io/simDAG/reference/node.md) calls.

- return_networks:

  Whether to also return networks that were included or generated due to
  the presence of
  [`network`](https://robindenz1.github.io/simDAG/reference/network.md)
  calls in the supplied `dag` or not. If set to `TRUE`, a named list of
  length 2 will be returned instead of only returning the generated
  data. Defaults to `FALSE`.

- check_inputs:

  Whether to perform plausibility checks for the user input or not. Is
  set to `TRUE` by default, but can be set to `FALSE` in order to speed
  things up when using this function in a simulation study or something
  similar.

## Details

***How it Works***:

First, `n_sim` i.i.d. samples from the root nodes are drawn. Children of
these nodes are then generated one by one according to specified
relationships and causal coefficients. For example, lets suppose there
are two root nodes, `age` and `sex`. Those are generated from a normal
distribution and a bernoulli distribution respectively. Afterward, the
child node `height` is generated using both of these variables as
parents according to a linear regression with defined coefficients,
intercept and sigma (random error). This works because every DAG has at
least one topological ordering, which is a linear ordering of vertices
such that for every directed edge \\u\\ \\v\\, vertex \\u\\ comes before
\\v\\ in the ordering. By using `sort_dag=TRUE` it is ensured that the
nodes are processed in such an ordering.

This procedure is simple in theory, but can get very complex when
manually coded. This function offers a simplified workflow by only
requiring the user to define the `dag` object with appropriate
information (see documentation of
[`node`](https://robindenz1.github.io/simDAG/reference/node.md)
function). A sample of size `n_sim` is then generated from the DAG
specified by those two arguments.

***Specifying the DAG***:

Concrete details on how to specify the needed `dag` object are given in
the documentation page of the
[`node`](https://robindenz1.github.io/simDAG/reference/node.md) and
[`network`](https://robindenz1.github.io/simDAG/reference/network.md)
functions and in the vignettes of this package.

***Can this function create longitudinal data?***

Yes and no. It theoretically can, but only if the user-specified `dag`
directly specifies a node for each desired point in time. Using the
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
or
[`sim_discrete_event`](https://robindenz1.github.io/simDAG/reference/sim_discrete_event.md)
functions is better in some cases. A brief discussion about this topic
can be found in the vignettes of this package.

If time-dependent nodes were added to the `dag` using
[`node_td`](https://robindenz1.github.io/simDAG/reference/node.md)
calls, this function may not be used. Only the
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
and
[`sim_discrete_event`](https://robindenz1.github.io/simDAG/reference/sim_discrete_event.md)
functions will work in that case.

***Networks-Based Simulation***

In some cases the assumption that observations (rows) are independent
from each other is not sufficient. This function allows to relax this
assumption by directly supporting network-based dependencies among
individuals. Users may specify one or multiple networks of dependencies
between individuals and add those to the `dag` using the
[`network`](https://robindenz1.github.io/simDAG/reference/network.md)
function. It is then possible to use the
[`net`](https://robindenz1.github.io/simDAG/reference/net.md) function
inside the `formula` argument of
[`node`](https://robindenz1.github.io/simDAG/reference/node.md) calls to
directly make the value of that node dependent on some other variable
values of its' neighbors in the network. See the documentation and the
associated vignette for more information.

## Author

Robin Denz

## Value

If `return_networks=FALSE`, returns a single `data.table` including the
simulated data with (at least) one column per node specified in `dag`
and `n_sim` rows. Otherwise it returns a named list containing the
`data` and the `networks` supplied or generated through the course of
the simulation.

## See also

[`empty_dag`](https://robindenz1.github.io/simDAG/reference/empty_dag.md),
[`node`](https://robindenz1.github.io/simDAG/reference/node.md),
[`network`](https://robindenz1.github.io/simDAG/reference/network.md),
[`plot.DAG`](https://robindenz1.github.io/simDAG/reference/plot.DAG.md),
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)

## References

Denz, Robin and Nina Timmesfeld (2025). Simulating Complex Crossectional
and Longitudinal Data using the simDAG R Package. arXiv preprint, doi:
10.48550/arXiv.2506.01498.

## Examples

``` r
library(simDAG)

set.seed(345345)

dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("bmi", type="gaussian", parents=c("sex", "age"),
       betas=c(1.1, 0.4), intercept=12, error=2)

sim_dat <- sim_from_dag(dag=dag, n_sim=1000)

# More examples for each directly supported node type as well as for custom
# nodes can be found in the documentation page of the respective node function
```
