# simDAG 0.1.0

* This is the first release of this package

# simDAG 0.1.1

* Minor changes to documentation
* dag2matrix() now returns a numeric matrix even if the dag object contains only root nodes
* node() and node_td() now support character vectors in the 'name' argument, allowing easy creation of multiple nodes with the same definition
* Add sim_n_datasets() function
* Fix Bug in node_time_to_event() function that lead to the `immunity_duration` parameter being used incorrectly
