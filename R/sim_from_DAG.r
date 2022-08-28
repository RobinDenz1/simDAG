
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
#' @export
sim_from_dag <- function(n_sim, root_nodes, child_nodes, sort_dag=TRUE) {

  # sample from root nodes
  if (!is.data.frame(root_nodes)) {
    data <- vector(mode="list", length=length(root_nodes))
    for (i in seq_len(length(root_nodes))) {
      # add n to existing arguments
      args <- root_nodes[[i]]$params
      args$n <- n_sim

      # call data generation function
      out <- data.frame(do.call(get(root_nodes[[i]]$dist), args))
      colnames(out) <- root_nodes[[i]]$name

      data[[i]] <- out
    }
    data <- dplyr::bind_cols(data)
    data <- setDT(data)
  } else {
    data <- setDT(root_nodes)
  }

  # if not already ordered properly, use topological
  # sorting to get the right data generation sequence
  if (sort_dag) {
    requireNamespace("Rfast")
    adjacency_mat <- nodes2adjacency_mat(child_nodes=child_nodes,
                                         root_nodes=NULL)
    index_children <- Rfast::topological_sort(adjacency_mat)
  } else {
    index_children <- seq_len(length(child_nodes))
  }

  # go through DAG step by step
  for (i in index_children) {
    # get relevant arguments
    args <- child_nodes[[i]]
    args$data <- data
    args$type <- NULL
    if (child_nodes[[i]]$type!="cox") {
      args$name <- NULL
    }

    # call needed node function
    node_out <- do.call(get(paste0("node_", child_nodes[[i]]$type)), args)
    data <- add_node_to_data(data=data, new=node_out,
                             name=child_nodes[[i]]$name)
  }
  return(data)
}
