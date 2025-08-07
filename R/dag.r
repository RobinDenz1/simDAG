
## initialize an empty dag
#' @export
empty_dag <- function() {

  dag <- list(root_nodes=list(),
              child_nodes=list(),
              tx_nodes=list(),
              networks=list(),
              td_networks=list())
  class(dag) <- "DAG"

  return(dag)
}

## add one DAG.node object to a DAG object
#' @export
add_node <- function(dag, node) {

  # check if correct object types
  if (!(inherits(node, "DAG.node") | inherits(node, "DAG.network"))) {
    stop("'node' must be a DAG.node object created using either the node() or",
         " node_td() function or a DAG.network object created using the",
         " network() or network_td() function.", call.=FALSE)
  } else if (!inherits(dag, "DAG")) {
    stop("'dag' must be a DAG object created using the empty_dag() function.",
         call.=FALSE)
  }

  dag_names <- names_DAG(dag)

  if (!inherits(node, "DAG.network") && node$name %in% dag_names &&
      (!node$time_varying | (node$time_varying &
       node$type_str %in% c("time_to_event", "competing_events")))) {
    stop("A node / network with the name ", node$name,
         " is already present in the DAG object and will not be overwritten.",
         call.=FALSE)
  }

  # add network to networks or td_networks lists inside DAG
  if (inherits(node, "DAG.network")) {
    if (node$time_varying) {
      node$..index.. <- length(dag$tx_nodes) + length(dag$td_networks) + 1
      dag$td_networks[[node$name]] <- node
    } else {
      node$..index.. <- length(dag$child_nodes) + length(dag$networks) + 1
      dag$networks[[node$name]] <- node
    }
  }

  # add node to child, root or tx_nodes lists inside DAG
  if (inherits(node, "DAG.node")) {
    if (node$time_varying) {
      node$..index.. <- length(dag$tx_nodes) + length(dag$td_networks) + 1
      dag$tx_nodes[[length(dag$tx_nodes) + 1]] <- node
    } else if (length(node$parents) == 0 || all(node$parents=="")) {
      dag$root_nodes[[length(dag$root_nodes) + 1]] <- node
    } else {
      node$..index.. <- length(dag$child_nodes) + length(dag$networks) + 1
      dag$child_nodes[[length(dag$child_nodes) + 1]] <- node
    }
  }

  # if not acyclic after adding node, return error
  g <- as.igraph(x=dag, include_root_nodes=TRUE, include_td_nodes=FALSE,
                 include_networks=TRUE)

  if (!igraph::is_acyclic(g)) {
    cycle <- paste0(find_cycle(graph=g, start=node$name), collapse=" -> ")
    stop("Adding node '", node$name, "' as specified is impossible, ",
         "because it would make the DAG cyclic through the path:\n",
         cycle, call.=FALSE)
  }

  return(dag)
}

## S3 plus method for DAG objects
#' @export
`+.DAG` <- function(object_1, object_2) {

  if (inherits(object_1, "DAG") &
      inherits(object_2, c("DAG.node", "DAG.network")) &
      is.character(object_2[[1]])) {
    out <- add_node(dag=object_1, node=object_2)
  } else if (inherits(object_1, "DAG") &
             inherits(object_2, c("DAG.node", "DAG.network"))) {
    out <- object_1
    for (i in seq_len(length(object_2))) {
      out <- add_node(dag=out, node=object_2[[i]])
    }
  } else if (inherits(object_1, c("DAG.node", "DAG.network")) &
             inherits(object_2, "DAG") &
             is.character(object_1[[1]])) {
    out <- add_node(dag=object_2, node=object_1)
  } else if (inherits(object_1, c("DAG.node", "DAG.network")) &
             inherits(object_2, "DAG")) {
    out <- object_2
    for (i in seq_len(length(object_1))) {
      out <- add_node(dag=out, node=object_1[[i]])
    }
  } else {
    stop("Only output created using the node(), node_td(), network() and",
         " network_td() functions can be added to a DAG object.", call.=FALSE)
  }
  return(out)
}

## S3 print method for DAG objects
#' @export
print.DAG <- function(x, ...) {

  root_names <- names_DAG_level(x, "root")
  child_names <- names_DAG_level(x, "child")
  tx_names <- names_DAG_level(x, "tx")

  # remove names from roots / children that are also time-varying
  root_names <- root_names[!root_names %in% tx_names]
  child_names <- child_names[!child_names %in% tx_names]

  n_root_nodes <- length(root_names)
  n_child_nodes <- length(child_names)
  n_tx_nodes <- length(tx_names)
  n_total_nodes <- n_root_nodes + n_child_nodes + n_tx_nodes
  n_networks <- length(x$networks)

  if (is_empty_dag(x)) {
    cat("An empty DAG object without any nodes.\n")
  } else {
    cat("A DAG object with:\n")
    cat("  - ", n_total_nodes, " nodes in total\n")
    cat("  - ", n_root_nodes, " of which are root nodes\n")
    cat("  - ", n_child_nodes, " of which are child nodes\n")
    cat("  - ", n_tx_nodes, " of which are time-varying nodes\n")
    cat("  - ", n_networks, " imposed network structure(s)\n")
  }
}

## S3 summary method for DAG objects
#' @export
summary.DAG <- function(object, char_max=60, ...) {

  if (is_empty_dag(object)) {
    cat("An empty DAG object without any nodes.\n")
    return(invisible(character(0)))
  } else {
    # setup output vector
    names_dag <- names_DAG(object, remove_duplicates=FALSE,
                           include_tx_nodes=TRUE)
    str_len <- numeric()
    str_equations <- character()

    # loop over all node types and all nodes therein,
    # obtaining the structural equations for each one
    # NOTE: because some nodes have multiple equations, extra loop with k
    for (i in seq_len(3)) {
      for (j in seq_len(length(object[[i]]))) {
        str_equations_i <- structural_equation(object[[i]][[j]])
        str_len[length(str_len) + 1] <- length(str_equations_i)
        for (k in seq_len(length(str_equations_i))) {
          str_equations[length(str_equations) + 1] <- str_equations_i[k]
        }
      }
    }
    names(str_equations) <- rep(names_dag, times=str_len)

    str_equations_print <- align_str_equations(str_equations)
    str_equations_print <- vapply(str_equations_print,
                                  FUN=add_line_breaks,
                                  FUN.VALUE=character(1),
                                  char_max=char_max)

    cat("A DAG object using the following structural equations:\n\n")
    cat(str_equations_print, sep="\n")

    return(invisible(str_equations))
  }
}

## S3 method for DAG object to turn a DAG into an igraph object
#' @importFrom igraph as.igraph
#' @export
as.igraph.DAG <- function(x, include_root_nodes=TRUE, include_td_nodes=TRUE,
                          include_networks=FALSE, ...) {
  mat <- dag2matrix(dag=x, include_root_nodes=include_root_nodes,
                    include_td_nodes=include_td_nodes,
                    include_networks=include_networks)
  g <- igraph::graph_from_adjacency_matrix(mat, mode="directed")
  return(g)
}

# get names of nodes in a DAG at a given level (root, child, tx)
names_DAG_level <- function(dag, level) {

  if (level=="root") {
    nodes <- dag$root_nodes
  } else if (level=="child") {
    nodes <- dag$child_nodes
  } else if (level=="tx") {
    nodes <- dag$tx_nodes
  }
  names <- vapply(nodes, FUN=function(x){x$name}, FUN.VALUE=character(1))
  return(names)
}

## extract all node names from DAG objects
## NOTE: not an S3 method because that makes it confusing for R-Studio users
names_DAG <- function(x, include_tx_nodes=FALSE, remove_duplicates=TRUE) {
  root_names <- names_DAG_level(x, "root")
  child_names <- names_DAG_level(x, "child")
  tx_names <- names_DAG_level(x, "tx")

  if (include_tx_nodes) {
    out <- c(root_names, child_names, tx_names)
  } else {
    out <- c(root_names, child_names)
  }

  # in case the node is defined in both fixed and varying
  if (remove_duplicates) {
    out <- out[!duplicated(out)]
  }

  return(out)
}

## check if DAG object contains time-varying nodes
is_time_varying_dag <- function(dag) {
  length(dag$tx_nodes)!=0
}

## check if DAG object is empty
is_empty_dag <- function(dag) {
  (length(dag$root_nodes) + length(dag$child_nodes) + length(dag$tx_nodes))==0
}
