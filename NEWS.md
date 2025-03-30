# simDAG 0.1.0

* This is the first release of this package

# simDAG 0.1.1

Enhancements

* `node()` and `node_td()` now support character vectors in the 'name' argument, allowing easy creation of multiple nodes with the same definition

Bug Fixes

* There was a bug in the `node_time_to_event()` function that lead to the `immunity_duration` parameter being used incorrectly. Since events were still recorded correctly, this was only apparent when using `save_states="all"`. Works correctly now.
* There was a small bug in `dag2matrix()` if the dag object contained only root nodes. In this case, a logical matrix was returned. Now it returns the correct numeric matrix.

New Features

* Added the `sim_n_datasets()` function to generate multiple datasets from a single dag object, possibly using multicore processing

Documentation

* Minor changes to documentation pages
* Minor changes to vignettes

# simDAG 0.1.2

General

* `simDAG` no longer lists `data.table` under "Depends" in the description file. It is instead listed under "Imports" as recommended by the `data.table` crew

Enhancements

* Print underlying structural equations in `summary.DAG()` and `summary.DAG.node()`
* Added the `overlap` argument to both `long2start_stop()` and `sim2data()` to directly create start-stop data with overlapping durations, as needed for some statistical models
* Added the `target_event` and `keep_only_first` arguments to `sim2data()` and related functions, to allow direct transformation into model-ready dataset
* Made the `long2start_stop()` function more computationally efficient

Bug Fixes

* Fixed a small bug in input checks of the `node_time_to_event()` function, which printed an error when not all arguments to `prob_fun` were supplied, even when these arguments had default values
* Fixed bug in `print.DAG.node()` which occurred when a time-to-event node with no parents was supplied
* Fixed a bug in `sim2data()` which lead to inconsistent results when `event_duration=0` was used in one or more nodes of type "time_to_event" or "competing_events". This made me realize that event durations smaller than 1 make no sense. They are now no longer allowed and the default of the node types has been changed accordingly.
* Fixed a bug that resulted in errors when interaction terms were being supplied through `formula` objects of child nodes 

New Features

* Added the `as.igraph.DAG()` method which extends the generic function `as.igraph()` to conveniently parse `DAG` objects to `igraph` objects
* Added `as.data.table.simDT()` and `as.data.frame.simDT()` for user convenience

Documentation

* Documentation pages of most nodes now include formal descriptions of the data generation mechanism
* Re-wrote the documentation page for custom node definition

# simDAG 0.2.0

Enhancements

* Added the `output` argument to the `rbernoulli()` function to allow different output formats.
* Change default of `sort_dag` in `sim_from_dag()` from `TRUE` to `FALSE`.
* Moved the functionality of the `coerce2factor` and `coerce2numeric` arguments in `rcategorical()`, `node_multinomial()` and `node_binomial()` to the `output` argument for a more consistent syntax and easier usage.
* It is now allowed to directly supply functions to the `type` argument in `node()` and `node_td()`.
* Passing optional arguments to the `layout` function in `plot.DAG()` is now supported.

Bug Fixes

* The `node_fill` argument of the `plot.DAG()` function is no longer being ignored if `mark_td_nodes` was set to `TRUE`.

New Features

* Added a new enhanced formula interface, which allows users to directly type out the underlying structural equations for many node types. This replaces the old `formula` argument. Standard formulas (without betas and intercepts) are still supported, but no longer mentioned in the documentation and will be deprecated in future versions.

Documentation

* Added a new vignette explaining in detail how the new enhanced formula interface for the `node()` function works.

# simDAG 0.2.1

Enhancements

* External variables may now be used in the formula interface using `eval()` calls.
* Added the `remove_not_at_risk` argument to the `sim2data()` function.
* Change default of `t0_sort_dag` in `sim_discrete_time()` from `TRUE` to `FALSE` for more consistency with `sim_from_dag()`.

Bug Fixes

* Using `sim2data()` with time-dependent nodes of type `node_competing_events()` no longer results in an unwarranted error message.
* Using the column name `"time"` in the `time` argument of `long2start_stop()` now works properly.

New Features

* Added the `node_identity()` function to allow users to directly calculate nodes as an R expression of other nodes without the need to define a new function.

Documentation

* Added more examples in the formula vignette.

# simDAG 0.2.2

Bug Fixes

* When specifying a node as both a root or child node and as a time-dependent node, it is no longer counted twice in `print.DAG()`
* Fix small error in tests due to changes in `data.table`

# simDAG 0.3.0

New Features

* Support for random effects and random slopes (mixed model syntax) has been added to the `formula` interface of `node()` and `node_td()` when using nodes of type `"gaussian"`, `"binomial"` or `"poisson"`. 
* Added seven new node types: `node_aftreg()`, `node_ahreg()`, `node_poreg()`, `node_ypreg()`, `node_ehreg()`, `node_zeroinfl()` and `node_mixture()`.

Enhancements

* Added the `reference` argument to `rbernoulli()` and `rcategorical()` to make it easier to specify the reference category when coding the output as a factor variable.
* `+.DAG` now checks whether the `DAG` would become cyclic when adding a `node()` and returns an error if it does.
* Added the `include_td_nodes` and `include_root_nodes` arguments to `as.igraph.DAG()`.
* Changed the default of `n_cores` in the `sim_n_datasets()` function to 1 from `parallel::detectCores()`
* Function input to the `cens_dist` argument in the `node_cox()` function is now allowed.
* The argument `as_two_cols` was added to the `node_cox()` function to allow users to return only the time-to-event as a single column if no censoring is applied.

Bug Fixes

* Fixed a bug that occurred when a user-specified root node function returned a `data.frame`-like object with more than one column.
* Fixed a bug when printing the structural equation of a poisson node, in which the `exp()` call did not show up when the node was defined using the `formula` argument.

Documentation

* Added new "cookbook" vignette to showcase more possible use cases of the package.
* Visual update of the DAGs shown in the vignette figures.
