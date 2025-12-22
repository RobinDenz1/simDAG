# Changelog

## simDAG 0.4.2

New Features

- Added the
  [`sim_discrete_event()`](https://robindenz1.github.io/simDAG/reference/sim_discrete_event.md)
  function, which allows users to perform discrete-event simulations to
  generate complex longitudinal data in continuous time. This function
  is usually much faster than comparable
  [`sim_discrete_time()`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
  calls, although at the cost of some flexibility.
- Added the
  [`rtexp()`](https://robindenz1.github.io/simDAG/reference/rtexp.md)
  function to allow sampling from left-truncated exponential
  distributions.
- Added the
  [`rsample()`](https://robindenz1.github.io/simDAG/reference/rsample.md)
  function as a convenient wrapper around
  [`sample()`](https://rdrr.io/r/base/sample.html), as suggested by Ed
  Hagen.

Enhancements

- Added the `remove_if` and `break_if` arguments to the
  [`sim_discrete_time()`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
  function, to allow users some options that potentially make the
  simulation much faster.

Documentation

- Small changes to include the new
  [`sim_discrete_event()`](https://robindenz1.github.io/simDAG/reference/sim_discrete_event.md)
  everywhere.
- Added a new vignette introducing the
  [`sim_discrete_event()`](https://robindenz1.github.io/simDAG/reference/sim_discrete_event.md)
  function.

## simDAG 0.4.1

CRAN release: 2025-10-15

New Features

- Added the `link` argument to
  [`node_gaussian()`](https://robindenz1.github.io/simDAG/reference/node_gaussian.md),
  [`node_binomial()`](https://robindenz1.github.io/simDAG/reference/node_binomial.md),
  [`node_poisson()`](https://robindenz1.github.io/simDAG/reference/node_poisson.md),
  [`node_negative_binomial()`](https://robindenz1.github.io/simDAG/reference/node_negative_binomial.md)
  and
  [`node_zeroinfl()`](https://robindenz1.github.io/simDAG/reference/node_zeroinfl.md)
  to allow different link functions when generating data from these
  nodes.
- Added the
  [`as.dagitty.DAG()`](https://robindenz1.github.io/simDAG/reference/as.dagitty.DAG.md)
  function to allow direct conversion of `DAG` objects to `dagitty`
  objects.

Bug Fixes

- Previously, the
  [`sim_n_datasets()`](https://robindenz1.github.io/simDAG/reference/sim_n_datasets.md)
  function used `stats::runif(1)` as a default for the `seed` argument.
  Because seeds are coerced to integers in
  [`set.seed()`](https://rdrr.io/r/base/Random.html), this essentially
  meant the `seed` argument was always set to 0 (unless changed by the
  user), which was not intended. We changed the default to be `NULL`,
  which is equivalent to not setting a `seed`. This might change results
  obtained using previous versions. To get the same result as in
  previous versions, use `seed=0` or `seed=stats::runif(1)`.
- Fixed a bug that occurred when calling
  [`node()`](https://robindenz1.github.io/simDAG/reference/node.md) or
  [`node_td()`](https://robindenz1.github.io/simDAG/reference/node.md)
  inside a function with objects passed to `parents` or `formula`.

Documentation

- Added explanations for how to simulate data for Cox models with
  time-varying covariates and Aalen additive hazards models with
  time-varying covariates to the cookbook vignette.

## simDAG 0.4.0

CRAN release: 2025-08-27

New Features

- Added the
  [`network()`](https://robindenz1.github.io/simDAG/reference/network.md),
  [`network_td()`](https://robindenz1.github.io/simDAG/reference/network.md)
  and [`net()`](https://robindenz1.github.io/simDAG/reference/net.md)
  functions to allow simulations with network based dependencies among
  individuals. This includes static and dynamic networks in regular
  `DAG`s and discrete-time simulations.

Enhancements

- Added the `kind` argument to
  [`node_identity()`](https://robindenz1.github.io/simDAG/reference/node_identity.md)
  to allow different kinds of input to the `formula` argument.
- Added the `include_networks` argument to
  [`dag2matrix()`](https://robindenz1.github.io/simDAG/reference/dag2matrix.md)
  and [`as.igraph()`](https://r.igraph.org/reference/as.igraph.html),
  due to the new networks-based simulation features.
- Added the `unif` argument to
  [`node_time_to_event()`](https://robindenz1.github.io/simDAG/reference/node_time_to_event.md)
  to allow users to generate multiple time-to-event nodes at once that
  are basically using the same “seed” value for the random number
  generator.

Bug Fixes

- Fixed a minor bug that occurred when using `node + DAG` (in this
  order).
- Fixed a faulty error message produced when using `formula` with just
  one predictor in nodes that do not need an intercept.
- Fixed a faulty error message when using a string of a `formula` with
  [`node_identity()`](https://robindenz1.github.io/simDAG/reference/node_identity.md)
  in
  [`sim_discrete_time()`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md).

Documentation

- Added a new vignette to describe the new networks-based simulation
  features.
- Updated existing documentation pages to include networks-based
  simulation features.

## simDAG 0.3.2

CRAN release: 2025-06-24

Documentation

- Added a vignette of the Journal of Statistical Software paper
  (provisionally accepted)
- Changed all citation information accordingly

## simDAG 0.3.1

CRAN release: 2025-05-28

Enhancements

- Added the `remove_vars` argument to the
  [`sim2data()`](https://robindenz1.github.io/simDAG/reference/sim2data.md)
  function, to allow users to exclude certain variables from the output
  if desired.

Bug Fixes

- Fixed a bug where
  [`sim_n_datasets()`](https://robindenz1.github.io/simDAG/reference/sim_n_datasets.md)
  would fail with `n_cores > 1` whenever nested custom functions were
  used in nodes.

Documentation

- The previous `node_custom` documentation page has been turned into a
  vignette (which it should have been from the start).

## simDAG 0.3.0

CRAN release: 2025-03-30

New Features

- Support for random effects and random slopes (mixed model syntax) has
  been added to the `formula` interface of
  [`node()`](https://robindenz1.github.io/simDAG/reference/node.md) and
  [`node_td()`](https://robindenz1.github.io/simDAG/reference/node.md)
  when using nodes of type `"gaussian"`, `"binomial"` or `"poisson"`.
- Added seven new node types:
  [`node_aftreg()`](https://robindenz1.github.io/simDAG/reference/node_rsurv.md),
  [`node_ahreg()`](https://robindenz1.github.io/simDAG/reference/node_rsurv.md),
  [`node_poreg()`](https://robindenz1.github.io/simDAG/reference/node_rsurv.md),
  [`node_ypreg()`](https://robindenz1.github.io/simDAG/reference/node_rsurv.md),
  [`node_ehreg()`](https://robindenz1.github.io/simDAG/reference/node_rsurv.md),
  [`node_zeroinfl()`](https://robindenz1.github.io/simDAG/reference/node_zeroinfl.md)
  and
  [`node_mixture()`](https://robindenz1.github.io/simDAG/reference/node_mixture.md).

Enhancements

- Added the `reference` argument to
  [`rbernoulli()`](https://robindenz1.github.io/simDAG/reference/rbernoulli.md)
  and
  [`rcategorical()`](https://robindenz1.github.io/simDAG/reference/rcategorical.md)
  to make it easier to specify the reference category when coding the
  output as a factor variable.
- `+.DAG` now checks whether the `DAG` would become cyclic when adding a
  [`node()`](https://robindenz1.github.io/simDAG/reference/node.md) and
  returns an error if it does.
- Added the `include_td_nodes` and `include_root_nodes` arguments to
  [`as.igraph.DAG()`](https://robindenz1.github.io/simDAG/reference/as.igraph.DAG.md).
- Changed the default of `n_cores` in the
  [`sim_n_datasets()`](https://robindenz1.github.io/simDAG/reference/sim_n_datasets.md)
  function to 1 from
  [`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html)
- Function input to the `cens_dist` argument in the
  [`node_cox()`](https://robindenz1.github.io/simDAG/reference/node_cox.md)
  function is now allowed.
- The argument `as_two_cols` was added to the
  [`node_cox()`](https://robindenz1.github.io/simDAG/reference/node_cox.md)
  function to allow users to return only the time-to-event as a single
  column if no censoring is applied.

Bug Fixes

- Fixed a bug that occurred when a user-specified root node function
  returned a `data.frame`-like object with more than one column.
- Fixed a bug when printing the structural equation of a poisson node,
  in which the [`exp()`](https://rdrr.io/r/base/Log.html) call did not
  show up when the node was defined using the `formula` argument.

Documentation

- Added new “cookbook” vignette to showcase more possible use cases of
  the package.
- Visual update of the DAGs shown in the vignette figures.

## simDAG 0.2.2

CRAN release: 2025-02-23

Bug Fixes

- When specifying a node as both a root or child node and as a
  time-dependent node, it is no longer counted twice in `print.DAG()`
- Fix small error in tests due to changes in `data.table`

## simDAG 0.2.1

CRAN release: 2025-01-07

Enhancements

- External variables may now be used in the formula interface using
  [`eval()`](https://rdrr.io/r/base/eval.html) calls.
- Added the `remove_not_at_risk` argument to the
  [`sim2data()`](https://robindenz1.github.io/simDAG/reference/sim2data.md)
  function.
- Change default of `t0_sort_dag` in
  [`sim_discrete_time()`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
  from `TRUE` to `FALSE` for more consistency with
  [`sim_from_dag()`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md).

Bug Fixes

- Using
  [`sim2data()`](https://robindenz1.github.io/simDAG/reference/sim2data.md)
  with time-dependent nodes of type
  [`node_competing_events()`](https://robindenz1.github.io/simDAG/reference/node_competing_events.md)
  no longer results in an unwarranted error message.
- Using the column name `"time"` in the `time` argument of
  [`long2start_stop()`](https://robindenz1.github.io/simDAG/reference/long2start_stop.md)
  now works properly.

New Features

- Added the
  [`node_identity()`](https://robindenz1.github.io/simDAG/reference/node_identity.md)
  function to allow users to directly calculate nodes as an R expression
  of other nodes without the need to define a new function.

Documentation

- Added more examples in the formula vignette.

## simDAG 0.2.0

CRAN release: 2024-09-03

Enhancements

- Added the `output` argument to the
  [`rbernoulli()`](https://robindenz1.github.io/simDAG/reference/rbernoulli.md)
  function to allow different output formats.
- Change default of `sort_dag` in
  [`sim_from_dag()`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
  from `TRUE` to `FALSE`.
- Moved the functionality of the `coerce2factor` and `coerce2numeric`
  arguments in
  [`rcategorical()`](https://robindenz1.github.io/simDAG/reference/rcategorical.md),
  [`node_multinomial()`](https://robindenz1.github.io/simDAG/reference/node_multinomial.md)
  and
  [`node_binomial()`](https://robindenz1.github.io/simDAG/reference/node_binomial.md)
  to the `output` argument for a more consistent syntax and easier
  usage.
- It is now allowed to directly supply functions to the `type` argument
  in [`node()`](https://robindenz1.github.io/simDAG/reference/node.md)
  and
  [`node_td()`](https://robindenz1.github.io/simDAG/reference/node.md).
- Passing optional arguments to the `layout` function in
  [`plot.DAG()`](https://robindenz1.github.io/simDAG/reference/plot.DAG.md)
  is now supported.

Bug Fixes

- The `node_fill` argument of the
  [`plot.DAG()`](https://robindenz1.github.io/simDAG/reference/plot.DAG.md)
  function is no longer being ignored if `mark_td_nodes` was set to
  `TRUE`.

New Features

- Added a new enhanced formula interface, which allows users to directly
  type out the underlying structural equations for many node types. This
  replaces the old `formula` argument. Standard formulas (without betas
  and intercepts) are still supported, but no longer mentioned in the
  documentation and will be deprecated in future versions.

Documentation

- Added a new vignette explaining in detail how the new enhanced formula
  interface for the
  [`node()`](https://robindenz1.github.io/simDAG/reference/node.md)
  function works.

## simDAG 0.1.2

CRAN release: 2024-06-13

General

- `simDAG` no longer lists `data.table` under “Depends” in the
  description file. It is instead listed under “Imports” as recommended
  by the `data.table` crew

Enhancements

- Print underlying structural equations in `summary.DAG()` and
  `summary.DAG.node()`
- Added the `overlap` argument to both
  [`long2start_stop()`](https://robindenz1.github.io/simDAG/reference/long2start_stop.md)
  and
  [`sim2data()`](https://robindenz1.github.io/simDAG/reference/sim2data.md)
  to directly create start-stop data with overlapping durations, as
  needed for some statistical models
- Added the `target_event` and `keep_only_first` arguments to
  [`sim2data()`](https://robindenz1.github.io/simDAG/reference/sim2data.md)
  and related functions, to allow direct transformation into model-ready
  dataset
- Made the
  [`long2start_stop()`](https://robindenz1.github.io/simDAG/reference/long2start_stop.md)
  function more computationally efficient

Bug Fixes

- Fixed a small bug in input checks of the
  [`node_time_to_event()`](https://robindenz1.github.io/simDAG/reference/node_time_to_event.md)
  function, which printed an error when not all arguments to `prob_fun`
  were supplied, even when these arguments had default values
- Fixed bug in `print.DAG.node()` which occurred when a time-to-event
  node with no parents was supplied
- Fixed a bug in
  [`sim2data()`](https://robindenz1.github.io/simDAG/reference/sim2data.md)
  which lead to inconsistent results when `event_duration=0` was used in
  one or more nodes of type “time_to_event” or “competing_events”. This
  made me realize that event durations smaller than 1 make no sense.
  They are now no longer allowed and the default of the node types has
  been changed accordingly.
- Fixed a bug that resulted in errors when interaction terms were being
  supplied through `formula` objects of child nodes

New Features

- Added the
  [`as.igraph.DAG()`](https://robindenz1.github.io/simDAG/reference/as.igraph.DAG.md)
  method which extends the generic function
  [`as.igraph()`](https://r.igraph.org/reference/as.igraph.html) to
  conveniently parse `DAG` objects to `igraph` objects
- Added
  [`as.data.table.simDT()`](https://robindenz1.github.io/simDAG/reference/sim2data.md)
  and
  [`as.data.frame.simDT()`](https://robindenz1.github.io/simDAG/reference/sim2data.md)
  for user convenience

Documentation

- Documentation pages of most nodes now include formal descriptions of
  the data generation mechanism
- Re-wrote the documentation page for custom node definition

## simDAG 0.1.1

CRAN release: 2024-03-07

Enhancements

- [`node()`](https://robindenz1.github.io/simDAG/reference/node.md) and
  [`node_td()`](https://robindenz1.github.io/simDAG/reference/node.md)
  now support character vectors in the ‘name’ argument, allowing easy
  creation of multiple nodes with the same definition

Bug Fixes

- There was a bug in the
  [`node_time_to_event()`](https://robindenz1.github.io/simDAG/reference/node_time_to_event.md)
  function that lead to the `immunity_duration` parameter being used
  incorrectly. Since events were still recorded correctly, this was only
  apparent when using `save_states="all"`. Works correctly now.
- There was a small bug in
  [`dag2matrix()`](https://robindenz1.github.io/simDAG/reference/dag2matrix.md)
  if the dag object contained only root nodes. In this case, a logical
  matrix was returned. Now it returns the correct numeric matrix.

New Features

- Added the
  [`sim_n_datasets()`](https://robindenz1.github.io/simDAG/reference/sim_n_datasets.md)
  function to generate multiple datasets from a single dag object,
  possibly using multicore processing

Documentation

- Minor changes to documentation pages
- Minor changes to vignettes

## simDAG 0.1.0

CRAN release: 2023-08-28

- This is the first release of this package
