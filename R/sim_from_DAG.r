
## add output of a node function to data.frame
add_node_to_data <- function(data, new, name) {
  if (is.data.frame(new)) {
    new_names <- colnames(new)
    for (i in seq_len(length(new_names))) {
      data[[new_names[i]]] <- new[[new_names[i]]]
    }
  } else {
    data[, name] <- new
  }
  return(data)
}

## generate data from a DAG with defined nodes
#' @importFrom data.table data.table
#' @importFrom data.table setDT
#' @export
sim_from_dag <- function(dag, n_sim, sort_dag=TRUE, check_inputs=TRUE) {

  if (check_inputs) {
    check_inputs_sim_from_dag(dag=dag, n_sim=n_sim, sort_dag=sort_dag)
  }

  # sample from root nodes
  data <- vector(mode="list", length=length(dag$root_nodes))
  for (i in seq_len(length(dag$root_nodes))) {
    # add n to existing arguments
    args <- dag$root_nodes[[i]]$params
    args$n <- n_sim

    # call data generation function
    out <- data.table(do.call(get(dag$root_nodes[[i]]$type), args))
    colnames(out) <- dag$root_nodes[[i]]$name

    data[[i]] <- out
  }
  data <- dplyr::bind_cols(data)
  setDT(data)

  if (length(dag$child_nodes)==0) {
    return(data)
  }

  # if not already ordered properly, use topological
  # sorting to get the right data generation sequence
  if (sort_dag) {
    requireNamespace("Rfast")
    adjacency_mat <- dag2matrix(dag=dag, include_root_nodes=FALSE)
    index_children <- Rfast::topological_sort(adjacency_mat)
  } else {
    index_children <- seq_len(length(dag$child_nodes))
  }

  # go through DAG step by step
  for (i in index_children) {

    # get relevant arguments
    args <- dag$child_nodes[[i]]
    args$data <- data
    args$type <- NULL

    if (dag$child_nodes[[i]]$type!="cox") {
      args$name <- NULL
    }

    # call needed node function
    node_out <- do.call(get(paste0("node_", dag$child_nodes[[i]]$type)), args)
    data <- add_node_to_data(data=data, new=node_out,
                             name=dag$child_nodes[[i]]$name)
  }
  return(data)
}
