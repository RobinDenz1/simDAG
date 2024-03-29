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
