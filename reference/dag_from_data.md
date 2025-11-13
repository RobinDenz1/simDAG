# Fills a partially specified `DAG` object with parameters estimated from reference data

Given a partially specified `DAG` object, where only the `name`, `type`
and the `parents` are specified plus a `data.frame` containing
realizations of these nodes, return a fully specified `DAG` (with
beta-coefficients, intercepts, errors, ...). The returned `DAG` can be
used directly to simulate data with the
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
function.

## Usage

``` r
dag_from_data(dag, data, return_models=FALSE, na.rm=FALSE)
```

## Arguments

- dag:

  A partially specified `DAG` object created using the
  [`empty_dag`](https://robindenz1.github.io/simDAG/reference/empty_dag.md)
  and [`node`](https://robindenz1.github.io/simDAG/reference/node.md)
  functions. See
  [`?node`](https://robindenz1.github.io/simDAG/reference/node.md) for a
  more detailed description on how to do this. All nodes need to contain
  information about their `name`, `type` and `parents`. All other
  attributes will be added (or overwritten if already in there) when
  using this function. Currently does not support DAGs with
  time-dependent nodes added with the
  [`node_td`](https://robindenz1.github.io/simDAG/reference/node.md)
  function.

- data:

  A `data.frame` or `data.table` used to obtain the parameters needed in
  the `DAG` object. It needs to contain a column for every node
  specified in the `dag` argument.

- return_models:

  Whether to return a list of all models that were fit to estimate the
  information for all child nodes (elements in `dag` where the `parents`
  argument is not `NULL`).

- na.rm:

  Whether to remove missing values or not.

## Details

***How it works***:

It can be cumbersome to specify all the node information needed for the
simulation, especially when there are a lot of nodes to consider.
Additionally, if data is available, it is natural to fit appropriate
models to the data to get an empirical estimate of the node information
for the simulation. This function automates this process. If the user
has a reasonable DAG and knows the node types, this is a very fast way
to generate synthetic data that corresponds well to the empirical data.

All the user has to do is create a minimal `DAG` object including only
information on the `parents`, the `name` and the node `type`. For root
nodes, the required distribution parameters are extracted from the data.
For child nodes, regression models corresponding to the specified `type`
are fit to the data using the `parents` as independent covariates and
the `name` as dependent variable. All required information is extracted
from these models and added to the respective node. The output contains
a fully specified `DAG` object which can then be used directly in the
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
function. It may also include a list containing the fitted models for
further inspection, if `return_models=TRUE`.

***Supported root node types***:

Currently, the following root node types are supported:

- `"rnorm"`: Estimates parameters of a normal distribution.

- `"rbernoulli"`: Estimates the `p` parameter of a Bernoulli
  distribution.

- `"rcategorical"`: Estimates the class probabilities in a categorical
  distribution.

Other types need to be implemented by the user.

***Supported child node types***:

Currently, the following child node types are supported:

- `"gaussian"`: Estimates parameters for a node of type
  `"`[`gaussian`](https://robindenz1.github.io/simDAG/reference/node_gaussian.md)`"`.

- `"binomial"`: Estimates parameters for a node of type
  `"`[`binomial`](https://robindenz1.github.io/simDAG/reference/node_binomial.md)`"`.

- `"poisson"`: Estimates parameters for a node of type
  `"`[`poisson`](https://robindenz1.github.io/simDAG/reference/node_poisson.md)`"`.

- `"negative_binomial"`: Estimates parameters for a node of type
  `"`[`negative_binomial`](https://robindenz1.github.io/simDAG/reference/node_negative_binomial.md)`"`.

- `"conditional_prob"`: Estimates parameters for a node of type
  `"`[`conditional_prob`](https://robindenz1.github.io/simDAG/reference/node_conditional_prob.md)`"`.

Other types need to be implemented by the user.

***Support for custom nodes***:

The `sim_from_dag` function supports custom node functions, as described
in the associated vignette. It is impossible for us to directly support
these custom types in this function directly. However, the user can
extend this function easily to accommodate any of his/her custom types.
Similar to defining a custom node type, the user simply has to write a
function that returns a correctly specified `node.DAG` object, given the
named arguments `name`, `parents`, `type`, `data` and `return_model`.
The first three arguments should simply be added directly to the output.
The `data` should be used inside your function to fit a model or obtain
the required parameters in some other way. The `return_model` argument
should control whether the model should be added to the output (in a
named argument called `model`). The function name should be
`paste0("gen_node_", YOURTYPE)`. An examples is given below.

***Interactions & cubic terms***:

This function currently does not support the usage of interaction
effects or non-linear terms (such as using `A ~ B + I(B^2)` as a
formula). Instead, it will be assumed that all values in `parents` have
a linear effect on the respective node. For example, using
`parents=c("A", "B")` for a node named `"C"` will use the formula
`C ~ A + B`. If other behavior is desired, users need to integrate this
into their own custom function as described above.

## Value

A list of length two containing the new fully specified `DAG` object
named `dag` and a list of the fitted models (if `return_models=TRUE`) in
the object named `models`.

## Author

Robin Denz

## Examples

``` r
library(simDAG)

set.seed(457456)

# get some example data from a known DAG
dag <- empty_dag() +
  node("death", type="binomial", parents=c("age", "sex"), betas=c(1, 2),
       intercept=-10) +
  node("age", type="rnorm", mean=10, sd=2) +
  node("sex", parents="", type="rbernoulli", p=0.5) +
  node("smoking", parents=c("sex", "age"), type="binomial",
       betas=c(0.6, 0.2), intercept=-2)

data <- sim_from_dag(dag=dag, n_sim=1000)

# suppose we only know the causal structure and the node type:
dag <- empty_dag() +
  node("death", type="binomial", parents=c("age", "sex")) +
  node("age", type="rnorm") +
  node("sex", type="rbernoulli") +
  node("smoking", type="binomial", parents=c("sex", "age"))

# get parameter estimates from data
dag_full <- dag_from_data(dag=dag, data=data)

# can now be used to simulate data
data2 <- sim_from_dag(dag=dag_full$dag, n_sim=100)
```
