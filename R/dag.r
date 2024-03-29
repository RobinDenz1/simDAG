
## initialize an empty dag
#' @export
empty_dag <- function() {

  dag_obj <- list(root_nodes=list(),
                  child_nodes=list(),
                  tx_nodes=list())
  class(dag_obj) <- "DAG"

  return(dag_obj)
}

## add one DAG.node object to a DAG object
#' @export
add_node <- function(dag, node) {

  # check if correct object types
  if (!inherits(node, "DAG.node")) {
    stop("'node' must be a DAG.node object created using either the node() or",
         " node_td() function.")
  } else if (!inherits(dag, "DAG")) {
    stop("'dag' must be a DAG object created using the empty_dag() function.")
  }

  dag_names <- names_DAG(dag)

  if (node$name %in% dag_names & !node$time_varying) {
    stop("A node with the name ", node$name, " is alread present in the",
         " DAG object and will not be overwritten.")
  }

  # add to child, root or tx node lists inside DAG
  if (node$time_varying) {
    dag$tx_nodes[[length(dag$tx_nodes) + 1]] <- node
  } else if (length(node$parents) == 0 || all(node$parents=="")) {
    dag$root_nodes[[length(dag$root_nodes) + 1]] <- node
  } else {
    dag$child_nodes[[length(dag$child_nodes) + 1]] <- node
  }

  return(dag)
}

## S3 plus method for DAG objects
#' @export
`+.DAG` <- function(object_1, object_2) {

  if (inherits(object_1, "DAG") & inherits(object_2, "DAG.node") &
      is.character(object_2[[1]])) {
    out <- add_node(dag=object_1, node=object_2)
  } else if (inherits(object_1, "DAG") & inherits(object_2, "DAG.node")) {
    out <- object_1
    for (i in seq_len(length(object_2))) {
      out <- add_node(dag=out, node=object_2[[i]])
    }
  } else if (inherits(object_1, "DAG.node") & inherits(object_2, "DAG") &
             is.character(object_1[[1]])) {
    out <- add_node(dag=object_2, node=object_1)
  } else if (inherits(object_1, "DAG.node") & inherits(object_2, "DAG")) {
    out <- object_2
    for (i in seq_len(length(object_2))) {
      out <- add_node(dag=out, node=object_1[[i]])
    }
  } else {
    stop("Only output created using the node() or node_td() function can",
         " be added to a DAG object.")
  }
  return(out)
}

## S3 print method for DAG objects
#' @export
print.DAG <- function(x, ...) {

  n_root_nodes <- length(x$root_nodes)
  n_child_nodes <- length(x$child_nodes)
  n_tx_nodes <- length(x$tx_nodes)
  n_total_nodes <- n_root_nodes + n_child_nodes + n_tx_nodes

  if (n_total_nodes==0) {
    cat("An empty DAG object without any nodes.\n")
  } else {
    cat("A DAG object with:\n")
    cat("  - ", n_total_nodes, " nodes in total\n")
    cat("  - ", n_root_nodes, " of which are root nodes\n")
    cat("  - ", n_child_nodes, " of which are child nodes\n")
    cat("  - ", n_tx_nodes, " of which are time-varying nodes\n")
  }
}

## S3 summary method for DAG objects
#' @export
summary.DAG <- function(object, ...) {
  print.DAG(x=object, ...)
}

## extract all node names from DAG objects
## NOTE: not an S3 method because that makes it confusing for R-Studio users
names_DAG <- function(x, include_tx_nodes=FALSE) {
  root_names <- vapply(x$root_nodes, FUN=function(x){x$name},
                       FUN.VALUE=character(1))
  child_names <- vapply(x$child_nodes, FUN=function(x){x$name},
                        FUN.VALUE=character(1))
  tx_names <- vapply(x$tx_nodes, FUN=function(x){x$name},
                     FUN.VALUE=character(1))

  if (include_tx_nodes) {
    out <- c(root_names, child_names, tx_names)
  } else {
    out <- c(root_names, child_names)
  }

  # in case the node is defined in both fixed and varying
  out <- out[!duplicated(out)]

  return(out)
}

## check if DAG object contains time-varying nodes
is_time_varying_dag <- function(dag) {
  length(dag$tx_nodes)!=0
}
