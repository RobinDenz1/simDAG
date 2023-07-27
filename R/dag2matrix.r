
## obtain an adjacency matrix of the causal DAG from a DAG object
#' @export
dag2matrix <- function(dag, include_root_nodes=TRUE) {

  if (!inherits(dag, "DAG")) {
    stop("'dag' must be a DAG object created using the empty_dag() function",
         " and + syntax.")
  } else if (!(is.logical(include_root_nodes) &
               length(include_root_nodes)==1)) {
    stop("'include_root_nodes' must be either TRUE or FALSE.")
  } else if (is_time_varying_dag(dag)) {
    warning("This function currently does not support dag objects",
            " with time-varying nodes added using the node_td() function.",
            " The output will only contain the nodes added with node().")
  }

  # extract node names
  names_children <- vapply(dag$child_nodes, function(x){x$name},
                           FUN.VALUE=character(1))
  names_roots <- vapply(dag$root_nodes, function(x){x$name},
                        FUN.VALUE=character(1))

  if (include_root_nodes) {
    all_names <- c(names_roots, names_children)
  } else {
    all_names <- names_children
  }

  # initialize adjacency matrix
  adjacency_mat <- matrix(data=FALSE,
                          nrow=length(all_names),
                          ncol=length(all_names))
  rownames(adjacency_mat) <- all_names
  colnames(adjacency_mat) <- all_names

  # populate it according to the lists
  for (i in seq_len(length(dag$child_nodes))) {
    i_parents <- dag$child_nodes[[i]]$parents

    if (!(length(i_parents) == 0 || all(i_parents==""))) {
      for (j in seq_len(length(i_parents))) {
        if (i_parents[j] %in% all_names) {
          adjacency_mat[i_parents[j], dag$child_nodes[[i]]$name] <- 1
        }
      }
    }
  }
  return(adjacency_mat)
}
