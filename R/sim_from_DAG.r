
## obtain an adjacency matrix of the causal DAG from the
## specified node lists
nodes2adjacency_mat <- function(child_nodes, root_nodes=NULL) {
  # extract node names
  names_children <- vapply(child_nodes, function(x){x$name},
                           FUN.VALUE=character(1))

  if (!is.null(root_nodes)) {
    names_roots <- vapply(root_nodes, function(x){x$name},
                          FUN.VALUE=character(1))
    all_names <- c(names_roots, names_children)
  }

  # initialize adjacency matrix
  if (is.null(root_nodes)) {
    adjacency_mat <- matrix(data=FALSE,
                            nrow=length(names_children),
                            ncol=length(names_children))
    rownames(adjacency_mat) <- names_children
    colnames(adjacency_mat) <- names_children
  } else {
    adjacency_mat <- matrix(data=FALSE,
                            nrow=length(all_names),
                            ncol=length(all_names))
    rownames(adjacency_mat) <- all_names
    colnames(adjacency_mat) <- all_names
  }

  # populate it according to the lists
  for (i in seq_len(length(child_nodes))) {
    i_parents <- child_nodes[[i]]$parents
    for (j in seq_len(length(i_parents))) {
      if (!is.null(root_nodes) | (i_parents[j] %in% names_children)) {
        adjacency_mat[i_parents[j], child_nodes[[i]]$name] <- 1
      }
    }
  }
  return(adjacency_mat)
}

## add output of a node function to data.frame
add_node_to_data <- function(data, new, name) {
  if (is.data.frame(new)) {
    colnames(new) <- paste0(name, "_", colnames(new))
    data <- cbind(data, new)
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
  } else {
    data <- root_nodes
  }

  # if not already ordered properly, use topological
  # sorting to get the right data generation sequence
  if (sort_dag) {
    adjacency_mat <- nodes2adjacency_mat(child_nodes=child_nodes,
                                         root_nodes=NULL)
    # use topological sorting
    requireNamespace("Rfast")
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
    args$name <- NULL

    # call needed node function
    node_out <- do.call(get(paste0("node_", child_nodes[[i]]$type)), args)
    data <- add_node_to_data(data=data, new=node_out,
                             name=child_nodes[[i]]$name)
  }
  return(data)
}
