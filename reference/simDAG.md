# Simulate Data from a DAG and Associated Node Information

***What is this package about?***

This package aims to give a comprehensive framework to simulate static
and longitudinal data given a directed acyclic graph and some
information about each node. Our goal is to make this package as
user-friendly and intuitive as possible, while allowing extreme
flexibility and while keeping the underlying code as fast and RAM
efficient as possible.

***What features are included in this package?***

This package includes three main simulation functions: the
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
function, which can be used to simulate data from a previously defined
causal DAG and node information, the
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
function, which implements a framework to conduct discrete-time
simulations and the
[`sim_discrete_event`](https://robindenz1.github.io/simDAG/reference/sim_discrete_event.md)
function for discrete-event simulations. The former is very easy to use,
but cannot deal with time-varying variable easily. The latter two are a
little more difficult to use (usually requiring the user to write some
functions himself), but allow the simulation of arbitrarily complex
longitudinal data in discrete and continuous time.

Through a collection of implemented node types, this package allows the
user to generate data with a mix of binary, categorical, count and
time-to-event data. The
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
and
[`sim_discrete_event`](https://robindenz1.github.io/simDAG/reference/sim_discrete_event.md)
functions additionally enable the user to generate time-to-event data
with, if desired, a mix of competing events, recurrent events,
time-varying variables that influence each other and any types of
censoring.

The package also includes a few functions to transform resulting data
into multiple formats, to augment existing DAGs, to plot DAGs and to
plot a flow-chart of the data generation process.

All of the above mentioned features may also be combined with
networks-based simulation, in which user-specified network dependencies
among individuals may be used directly when specifying nodes. One or
multiple networks (directed or undirected, weighted or unweighted) that
may or may not change over time (possibly as a function of other
variables) are supported.

***What does a typical workflow using this package look like?***

Users should start by defining a `DAG` object using the
[`empty_dag`](https://robindenz1.github.io/simDAG/reference/empty_dag.md)
and [`node`](https://robindenz1.github.io/simDAG/reference/node.md)
functions. This `DAG` can then be passed to one of the two simulation
functions included in this package. More information on how to do this
can be found in the respective documentation pages and the three
vignettes of this package.

***When should I use `sim_from_dag` and when `sim_discrete_time`?***

If you want to simulate data that is easily described using a standard
DAG without time-varying variables, you should use the
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
function. If the DAG includes time-varying variables, but you only want
to consider a few points in time and can easily describe the relations
between those manually, you can still use the
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
function. If you want more complex data with time-varying variables,
particularly with time-to-event outcomes, you should consider using the
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
or
[`sim_discrete_event`](https://robindenz1.github.io/simDAG/reference/sim_discrete_event.md)
functions.

***What features are missing from this package?***

The package currently only implements some possible child nodes. In the
future we would like to implement more child node types, such as more
complex survival time models and extending the already existing support
for multilevel modeling to other node types.

***Why should I use this package instead of the simcausal package?***

The simcausal package was a big inspiration for this package. In
contrast to it, however, it allows quite a bit more flexibility. A big
difference is that this package includes a comprehensive framework for
discrete-time and discrete-event simulations and the simcausal package
does not.

***Where can I get more information?***

The documentation pages contain a lot of information, relevant examples
and some literature references. Additional examples can be found in the
vignettes of this package, which can be accessed using:

- [`vignette(topic="v_sim_from_dag", package="simDAG")`](https://robindenz1.github.io/simDAG/articles/v_sim_from_dag.md)

- [`vignette(topic="v_sim_discrete_time", package="simDAG")`](https://robindenz1.github.io/simDAG/articles/v_sim_discrete_time.md)

- [`vignette(topic="v_sim_discrete_event", package="simDAG")`](https://robindenz1.github.io/simDAG/articles/v_sim_discrete_event.md)

- [`vignette(topic="v_covid_example", package="simDAG")`](https://robindenz1.github.io/simDAG/articles/v_covid_example.md)

- [`vignette(topic="v_using_formulas", package="simDAG")`](https://robindenz1.github.io/simDAG/articles/v_using_formulas.md)

- [`vignette(topic="v_custom_nodes", package="simDAG")`](https://robindenz1.github.io/simDAG/articles/v_custom_nodes.md)

- [`vignette(topic="v_cookbook", package="simDAG")`](https://robindenz1.github.io/simDAG/articles/v_cookbook.md)

- [`vignette(topic="v_sim_networks", package="simDAG")`](https://robindenz1.github.io/simDAG/articles/v_sim_networks.md)

A separate (already peer-reviewed) article about this package has been
provisionally accepted in the *Journal of Statistical Software*. The
preprint version of this article is available on arXiv (Denz and
Timmesfeld 2025) and as a vignette in this package.

***I have a problem using the `sim_discrete_time` or
`sim_discrete_event` function***

The
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
and
[`sim_discrete_event`](https://robindenz1.github.io/simDAG/reference/sim_discrete_event.md)
functions can become difficult to use depending on what kind of data the
user wants to generate. For this reason we put in extra effort to make
the documentation and examples as clear and helpful as possible. Please
consult the relevant documentation pages and the vignettes before
contacting the authors directly with programming related questions that
are not clearly bugs in the code.

***I want to suggest a new feature / I want to report a bug. Where can I
do this?***

Bug reports, suggestions and feature requests are highly welcome. Please
file an issue on the official github page or contact the author directly
using the supplied e-mail address.

## References

Denz, Robin and Nina Timmesfeld (2025). Simulating Complex Crossectional
and Longitudinal Data using the simDAG R Package. arXiv preprint, doi:
10.48550/arXiv.2506.01498.

Banks, Jerry, John S. Carson II, Barry L. Nelson, and David M. Nicol
(2014). Discrete-Event System Simulation. Vol. 5. Edinburgh Gate:
Pearson Education Limited.

## Author

Robin Denz, \<robin.denz@rub.de\>
