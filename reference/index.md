# Package index

## DAGs

Create, manipulate and inspect DAGs

- [`empty_dag()`](https://robindenz1.github.io/simDAG/reference/empty_dag.md)
  :

  Initialize an empty `DAG` object

- [`node()`](https://robindenz1.github.io/simDAG/reference/node.md)
  [`node_td()`](https://robindenz1.github.io/simDAG/reference/node.md) :

  Create a node object for a `DAG`

- [`network()`](https://robindenz1.github.io/simDAG/reference/network.md)
  [`network_td()`](https://robindenz1.github.io/simDAG/reference/network.md)
  :

  Create a network object for a `DAG`

- [`net()`](https://robindenz1.github.io/simDAG/reference/net.md) :

  Specify Network Dependencies in a `DAG`

- [`add_node()`](https://robindenz1.github.io/simDAG/reference/add_node.md)
  [`` `+`( ``*`<DAG>`*`)`](https://robindenz1.github.io/simDAG/reference/add_node.md)
  :

  Add a `DAG.node` or a `DAG.network` object to a `DAG` object

- [`plot(`*`<DAG>`*`)`](https://robindenz1.github.io/simDAG/reference/plot.DAG.md)
  :

  Plot a `DAG` object

- [`as.igraph(`*`<DAG>`*`)`](https://robindenz1.github.io/simDAG/reference/as.igraph.DAG.md)
  :

  Transform a `DAG` object into an `igraph` object

- [`as.dagitty(`*`<DAG>`*`)`](https://robindenz1.github.io/simDAG/reference/as.dagitty.DAG.md)
  :

  Transform a `DAG` object into a `dagitty` object

- [`do()`](https://robindenz1.github.io/simDAG/reference/do.md) :

  Pearls do-operator for `DAG` objects

- [`dag2matrix()`](https://robindenz1.github.io/simDAG/reference/dag2matrix.md)
  :

  Obtain a Adjacency Matrix from a `DAG` object

- [`matrix2dag()`](https://robindenz1.github.io/simDAG/reference/matrix2dag.md)
  :

  Obtain a `DAG` object from a Adjacency Matrix and a List of Node Types

- [`dag_from_data()`](https://robindenz1.github.io/simDAG/reference/dag_from_data.md)
  :

  Fills a partially specified `DAG` object with parameters estimated
  from reference data

## Simulation

Simulate data from a DAG and manipulate or visualize the output

- [`sim_from_dag()`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
  :

  Simulate Data from a `DAG`

- [`sim_discrete_time()`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
  :

  Simulate Data from a `DAG` with Time-Dependent Variables

- [`sim_n_datasets()`](https://robindenz1.github.io/simDAG/reference/sim_n_datasets.md)
  :

  Simulate multiple datasets from a single `DAG` object

- [`sim2data()`](https://robindenz1.github.io/simDAG/reference/sim2data.md)
  [`as.data.table(`*`<simDT>`*`)`](https://robindenz1.github.io/simDAG/reference/sim2data.md)
  [`as.data.frame(`*`<simDT>`*`)`](https://robindenz1.github.io/simDAG/reference/sim2data.md)
  :

  Transform `sim_discrete_time` output into the start-stop, long- or
  wide-format

- [`plot(`*`<simDT>`*`)`](https://robindenz1.github.io/simDAG/reference/plot.simDT.md)
  : Plot a Flowchart for a Discrete-Time Simulation

## Root nodes

Functions that can be used as root nodes

- [`rbernoulli()`](https://robindenz1.github.io/simDAG/reference/rbernoulli.md)
  : Generate Random Draws from a Bernoulli Distribution
- [`rcategorical()`](https://robindenz1.github.io/simDAG/reference/rcategorical.md)
  : Generate Random Draws from a Discrete Set of Labels with Associated
  Probabilities
- [`rsample()`](https://robindenz1.github.io/simDAG/reference/rsample.md)
  : Sample values from a given vector
- [`rconstant()`](https://robindenz1.github.io/simDAG/reference/rconstant.md)
  : Use a single constant value for a root node
- [`rtexp()`](https://robindenz1.github.io/simDAG/reference/rtexp.md) :
  Sample values from a left-truncated exponential distribution

## Child nodes

Functions that can be used as child nodes

- [`node_gaussian()`](https://robindenz1.github.io/simDAG/reference/node_gaussian.md)
  : Generate Data from a (Mixed) Linear Regression Model
- [`node_binomial()`](https://robindenz1.github.io/simDAG/reference/node_binomial.md)
  : Generate Data from a (Mixed) Binomial Regression Model
- [`node_conditional_prob()`](https://robindenz1.github.io/simDAG/reference/node_conditional_prob.md)
  : Generate Data Using Conditional Probabilities
- [`node_conditional_distr()`](https://robindenz1.github.io/simDAG/reference/node_conditional_distr.md)
  : Generate Data by Sampling from Different Distributions based on
  Strata
- [`node_multinomial()`](https://robindenz1.github.io/simDAG/reference/node_multinomial.md)
  : Generate Data from a Multinomial Regression Model
- [`node_poisson()`](https://robindenz1.github.io/simDAG/reference/node_poisson.md)
  : Generate Data from a (Mixed) Poisson Regression Model
- [`node_negative_binomial()`](https://robindenz1.github.io/simDAG/reference/node_negative_binomial.md)
  : Generate Data from a Negative Binomial Regression Model
- [`node_zeroinfl()`](https://robindenz1.github.io/simDAG/reference/node_zeroinfl.md)
  : Generate Data from a Zero-Inflated Count Model
- [`node_identity()`](https://robindenz1.github.io/simDAG/reference/node_identity.md)
  : Generate Data based on an expression
- [`node_mixture()`](https://robindenz1.github.io/simDAG/reference/node_mixture.md)
  : Generate Data from a Mixture of Node Definitions
- [`node_cox()`](https://robindenz1.github.io/simDAG/reference/node_cox.md)
  : Generate Data from a Cox-Regression Model
- [`node_aftreg()`](https://robindenz1.github.io/simDAG/reference/node_rsurv.md)
  [`node_ahreg()`](https://robindenz1.github.io/simDAG/reference/node_rsurv.md)
  [`node_ehreg()`](https://robindenz1.github.io/simDAG/reference/node_rsurv.md)
  [`node_poreg()`](https://robindenz1.github.io/simDAG/reference/node_rsurv.md)
  [`node_ypreg()`](https://robindenz1.github.io/simDAG/reference/node_rsurv.md)
  : Generate Data from Parametric Survival Models

## Time-dependent nodes

Functions that can only be used as time-dependent nodes

- [`node_time_to_event()`](https://robindenz1.github.io/simDAG/reference/node_time_to_event.md)
  : Generate Data from repeated Bernoulli Trials in Discrete-Time
  Simulation
- [`node_competing_events()`](https://robindenz1.github.io/simDAG/reference/node_competing_events.md)
  : Generate Data with Multiple Mutually Exclusive Events in
  Discrete-Time Simulation

## Misc

- [`long2start_stop()`](https://robindenz1.github.io/simDAG/reference/long2start_stop.md)
  :

  Transform a `data.table` in the long-format to a `data.table` in the
  start-stop format

- [`simDAG-package`](https://robindenz1.github.io/simDAG/reference/simDAG.md)
  : Simulate Data from a DAG and Associated Node Information
