
## obtain an adjacency matrix of the causal DAG from a DAG object
#' @export
dag2matrix <- function(dag, include_root_nodes=TRUE, include_td_nodes=FALSE,
                       include_networks=FALSE) {

  if (!inherits(dag, "DAG")) {
    stop("'dag' must be a DAG object created using the empty_dag() function",
         " and + syntax.", call.=FALSE)
  } else if (!(is.logical(include_root_nodes) &
               length(include_root_nodes)==1)) {
    stop("'include_root_nodes' must be either TRUE or FALSE.", call.=FALSE)
  }

  # extract node names
  names_children <- vapply(dag$child_nodes, function(x){x$name},
                           FUN.VALUE=character(1))
  names_roots <- vapply(dag$root_nodes, function(x){x$name},
                        FUN.VALUE=character(1))
  names_td_nodes <- vapply(dag$tx_nodes, function(x){x$name},
                           FUN.VALUE=character(1))
  names_networks <- vapply(dag$networks, function(x){x$name},
                           FUN.VALUE=character(1))

  ## put together names & node lists
  all_names <- c()
  all_nodes <- list()

  # roots
  if (include_root_nodes) {
    all_names <- c(all_names, names_roots)
    all_nodes <- c(all_nodes, dag$root_nodes)
  }

  # children (always included)
  all_names <- c(all_names, names_children)
  all_nodes <- c(all_nodes, dag$child_nodes)

  # time-dependent nodes
  if (include_td_nodes) {
    all_names <- c(all_names, names_td_nodes)
    all_nodes <- c(all_nodes, dag$tx_nodes)
  }

  # networks
  if (include_networks) {
    all_names <- c(all_names, names(names_networks))
    all_nodes <- c(all_nodes, dag$networks)
  }

  # keep only unique due to possibility of multiple nodes with same name
  # when using td_nodes
  all_names <- all_names[!duplicated(all_names)]

  # initialize adjacency matrix
  adjacency_mat <- matrix(data=0,
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
