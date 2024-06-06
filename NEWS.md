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

Enhancements

* Print underlying structural equations in `summary.DAG()` and `summary.DAG.node()`
* Added the `overlap` argument to both `long2start_stop()` and `sim2data()` to directly create start-stop data with overlapping durations, as needed for some statistical models
* Added the `target_event` and `keep_only_first` arguments to `sim2data()` and related functions, to allow direct transformation into model-ready dataset

Bug Fixes

* Fixed a small bug in input checks of the `node_time_to_event()` function, which printed an error when not all arguments to `prob_fun` were supplied, even when these arguments had default values
* Fixed bug in `print.DAG.node()` which occurred when a time-to-event node with no parents was supplied
* Fixed a bug in `sim2data()` which lead to inconsistent results when `event_duration=0` was used in one or more nodes of type "time_to_event" or "competing_events". This made me realize that event durations smaller than 1 make no sense. They are now no longer allowed and the default of the node types has been changed accordingly.

New Features

* Added the `as.igraph.DAG()` method which extends the generic function `as.igraph()` to conveniently parse `DAG` objects to `igraph` objects
* Added `as.data.table.simDT()` and `as.data.frame.simDT()` for user convenience

Documentation

* Documentation pages of most nodes now include formal descriptions of the data generation mechanism
* Re-wrote the documentation page for custom node definition
