
## initialize an empty dag
#' @export
empty_dag <- function() {

  dag_obj <- list(root_nodes=list(),
                  child_nodes=list())
  class(dag_obj) <- "DAG"

  return(dag_obj)
}

## add one DAG.node object to a DAG object
#' @export
add_node <- function(dag, node) {

  # check if correct object types
  if (!inherits(node, "DAG.node")) {
    stop("'node' must be a DAG.node object created using the node() function.")
  } else if (!inherits(dag, "DAG")) {
    stop("'dag' must be a DAG object created using the empty_dag() function.")
  }

  # add to child or root node lists inside DAG
  if (length(node$parents) == 0 || all(node$parents=="")) {
    dag$root_nodes[[length(dag$root_nodes)+1]] <- node
  } else {
    dag$child_nodes[[length(dag$child_nodes)+1]] <- node
  }

  return(dag)
}

## S3 plus method for DAG objects
#' @export
`+.DAG` <- function(object_1, object_2) {

  if (inherits(object_1, "DAG") & inherits(object_2, "DAG.node")) {
    out <- add_node(dag=object_1, node=object_2)
  } else if (inherits(object_1, "DAG.node") & inherits(object_2, "DAG")) {
    out <- add_node(dag=object_2, node=object_1)
  } else {
    stop("Only output created using the node() function can be added to a",
         " DAG object.")
  }
  return(out)
}

## S3 print method for DAG objects
#' @export
print.DAG <- function(x, ...) {

  n_root_nodes <- length(x$root_nodes)
  n_child_nodes <- length(x$child_nodes)
  n_total_nodes <- n_root_nodes + n_child_nodes

  if (n_total_nodes==0) {
    cat("An empty DAG object without any nodes.\n")
  } else {
    cat("A DAG object with:\n")
    cat("  - ", n_total_nodes, " nodes in total\n")
    cat("  - ", n_root_nodes, " of which are root nodes\n")
    cat("  - ", n_child_nodes, " of which are child nodes\n")
  }
}

## S3 summary method for DAG objects
#' @export
summary.DAG <- function(object, ...) {
  print.DAG(x=object, ...)
}

## S3 plot method of DAG objects, giving a quick and dirty way to plot
## the DAG using the node lists
#' @importFrom rlang .data
#' @export
plot.DAG <- function(x, ...) {

  requireNamespace("ggplot2")
  requireNamespace("igraph")
  requireNamespace("ggraph")

  # obtain adjacency matrix
  adj_mat <- dag2matrix(dag=x, include_root_nodes=TRUE)

  # create graph object from it
  adj_graph <- igraph::graph.adjacency(adjmatrix=adj_mat, mode="directed")

  # plot it
  ggraph::ggraph(adj_graph, layout="graphopt") +
    ggraph::geom_edge_link(
      ggplot2::aes(start_cap=ggraph::label_rect(.data$node1.name),
                   end_cap=ggraph::label_rect(.data$node2.name)),
      arrow=ggplot2::arrow(length=ggplot2::unit(4, "mm"))) +
    ggraph::geom_node_label(ggplot2::aes(label=.data$name))
}
