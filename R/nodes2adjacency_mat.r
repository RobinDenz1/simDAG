
## obtain an adjacency matrix of the causal DAG from the
## specified node lists
# TODO: Do I actually need to include root nodes?
#' @export
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
