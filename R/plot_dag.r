
## a quick and dirty way to plot the DAG using the node lists
#' @export
plot_dag <- function(root_nodes, child_nodes) {

  requireNamespace("ggplot2")
  requireNamespace("igraph")
  requireNamespace("ggraph")

  # obtain adjacency matrix
  adj_mat <- nodes2adjacency_mat(root_nodes=root_nodes, child_nodes=child_nodes)

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
