
## obtain an adjacency matrix of the causal DAG from a DAG object
#' @export
dag2matrix <- function(dag, include_root_nodes=TRUE, include_td_nodes=FALSE) {

  if (!inherits(dag, "DAG")) {
    stop("'dag' must be a DAG object created using the empty_dag() function",
         " and + syntax.")
  } else if (!(is.logical(include_root_nodes) &
               length(include_root_nodes)==1)) {
    stop("'include_root_nodes' must be either TRUE or FALSE.")
  }

  # extract node names
  names_children <- vapply(dag$child_nodes, function(x){x$name},
                           FUN.VALUE=character(1))
  names_roots <- vapply(dag$root_nodes, function(x){x$name},
                        FUN.VALUE=character(1))
  names_td_nodes <- vapply(dag$tx_nodes, function(x){x$name},
                           FUN.VALUE=character(1))

  # put together names & node lists
  if (include_root_nodes & include_td_nodes) {
    all_names <- c(names_roots, names_children, names_td_nodes)
    all_nodes <- c(dag$root_nodes, dag$child_nodes, dag$tx_nodes)
  } else if (include_root_nodes & !include_td_nodes) {
    all_names <- c(names_roots, names_children)
    all_nodes <- c(dag$root_nodes, dag$child_nodes)
  } else if (!include_root_nodes & include_td_nodes) {
    all_names <- c(names_children, names_td_nodes)
    all_nodes <- c(dag$child_nodes, dag$tx_nodes)
  } else {
    all_names <- names_children
    all_nodes <- dag$child_nodes
  }

  # keep only unique due to possibility of multiple nodes with same name
  # when using td_nodes
  all_names <- all_names[!duplicated(all_names)]

  # initialize adjacency matrix
  adjacency_mat <- matrix(data=FALSE,
                          nrow=length(all_names),
                          ncol=length(all_names))
  rownames(adjacency_mat) <- all_names
  colnames(adjacency_mat) <- all_names

  # populate it according to the lists
  for (i in seq_len(length(all_nodes))) {
    i_parents <- all_nodes[[i]]$parents

    if (all_nodes[[i]]$time_varying) {
      i_parents <- prepare_td_node_parents(i_parents)
    }

    if (!(length(i_parents) == 0 || all(i_parents==""))) {
      for (j in seq_len(length(i_parents))) {
        if (i_parents[j] %in% all_names) {
          adjacency_mat[i_parents[j], all_nodes[[i]]$name] <- 1
        }
      }
    }
  }
  return(adjacency_mat)
}

## keep only real node name, no extra column names
prepare_td_node_parents <- function(parents) {

  cleaned <- gsub("_event|_time|_time_since_last|_event_count", "", parents)

  return(unique(cleaned))
}
