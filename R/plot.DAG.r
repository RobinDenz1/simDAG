
## get (x, y) coordinates for nodes in a DAG object
get_node_data <- function(graph, adj_mat, layout, r, ...) {

  layout_name <- paste0("layout_", layout)
  layout_fun <- utils::getFromNamespace(layout_name, "igraph")

  plotdata <- as.data.frame(layout_fun(graph=graph, ...))
  colnames(plotdata) <- c("x", "y")

  plotdata$name <- colnames(adj_mat)
  plotdata$r <- r

  return(plotdata)
}

## calculate points defining a line between two given circles
## data must contain three named columns: x, y, r
get_edge_data <- function(from, to, data, pad) {

  data$r <- data$r + pad

  x_vec <- data$x
  names(x_vec) <- data$name

  y_vec <- data$y
  names(y_vec) <- data$name

  r_vec <- data$r
  names(r_vec) <- data$name

  x1 <- x_vec[from]
  y1 <- y_vec[from]

  x2 <- x_vec[to]
  y2 <- y_vec[to]

  r1 <- r_vec[from]
  r2 <- r_vec[to]

  delta_y <- y2 - y1
  delta_x <- x2 - x1
  L <- sqrt(delta_x^2 + delta_y^2)
  r1L <- r1 / L
  r2L <- r2 / L

  arrow_dat <- data.frame(
    x = x1 + delta_x * r1L,
    xend =  x2 - delta_x * r2L,
    y = y1 + delta_y * r1L,
    yend = y2 - delta_y * r2L
  )

  return(arrow_dat)
}

## sets all values in a square matrix that are lying on
## the diagonal from top-left to bottom-right to 0
set_diagonal_to_0 <- function(mat) {
  for (i in seq_len(nrow(mat))) {
    mat[i, i] <- 0
  }
  return(mat)
}

## S3 plot method for DAG objects
#' @importFrom rlang .data
#' @export
plot.DAG <- function(x, layout="nicely", node_size=0.2, node_names=NULL,
                     node_color="black", node_fill="red", node_linewidth=0.5,
                     node_linetype="solid", node_alpha=1,
                     node_text_color="black", node_text_alpha=1,
                     node_text_size=8, node_text_family="sans",
                     node_text_fontface="bold", arrow_color="black",
                     arrow_linetype="solid", arrow_linewidth=1,
                     arrow_alpha=1, arrow_head_size=0.3,
                     arrow_head_unit="cm", arrow_type="closed",
                     arrow_node_dist=0.03, gg_theme=ggplot2::theme_void(),
                     include_td_nodes=TRUE, mark_td_nodes=TRUE, ...) {

  requireNamespace("ggplot2")
  requireNamespace("ggforce")
  requireNamespace("igraph")

  check_inputs_plot.DAG(dag=x, node_size=node_size, node_names=node_names,
                        arrow_node_dist=arrow_node_dist, gg_theme=gg_theme,
                        include_td_nodes=include_td_nodes)

  # adjacency matrix
  adj_mat <- dag2matrix(dag=x,
                        include_root_nodes=TRUE,
                        include_td_nodes=include_td_nodes)
  adj_mat <- set_diagonal_to_0(adj_mat)

  if (!is.null(node_names)) {
    colnames(adj_mat) <- node_names
    rownames(adj_mat) <- node_names
  }

  # igraph object
  graph <- igraph::graph_from_adjacency_matrix(adjmatrix=adj_mat,
                                               mode="directed")

  # node coordinates
  d_nodes <- get_node_data(graph=graph, adj_mat=adj_mat, layout=layout,
                           r=node_size, ...)

  # edge coordinates
  d_edgelist <- igraph::as_edgelist(graph=graph)
  d_edges <- get_edge_data(from=d_edgelist[,1], to=d_edgelist[,2],
                           data=d_nodes, pad=arrow_node_dist)

  # plot nodes
  p <- ggplot2::ggplot(data=NULL)

  if (mark_td_nodes && is_time_varying_dag(x)) {
    tx_names <- vapply(x$tx_nodes, FUN=function(x){x$name},
                       FUN.VALUE=character(1))
    d_nodes$td_node <- d_nodes$name %in% tx_names

    p <- p + ggforce::geom_circle(data=d_nodes,
                              ggplot2::aes(x0=.data$x, y0=.data$y, r=.data$r,
                                           fill=.data$td_node),
                              color=node_color,
                              linewidth=node_linewidth,
                              linetype=node_linetype, alpha=node_alpha)
  } else {
    p <- p + ggforce::geom_circle(data=d_nodes,
                              ggplot2::aes(x0=.data$x, y0=.data$y, r=.data$r),
                              color=node_color, fill=node_fill,
                              linewidth=node_linewidth,
                              linetype=node_linetype, alpha=node_alpha)
  }


  # add names
  p <- p + ggplot2::geom_text(data=d_nodes,
                              ggplot2::aes(x=.data$x, y=.data$y,
                                           label=.data$name),
                     color=node_text_color, family=node_text_family,
                     size=node_text_size, fontface=node_text_fontface,
                     alpha=node_text_alpha)

  # add arrows between
  p <- p + ggplot2::geom_segment(data=d_edges,
                                 ggplot2::aes(x=.data$x, xend=.data$xend,
                                              y=.data$y, yend=.data$yend),
                                 arrow=ggplot2::arrow(
                                   length=ggplot2::unit(arrow_head_size,
                                                        arrow_head_unit),
                                   type=arrow_type),
                                 color=arrow_color, linetype=arrow_linetype,
                                 linewidth=arrow_linewidth, alpha=arrow_alpha)

  p <- p + gg_theme

  if (mark_td_nodes) {
    p <- p + ggplot2::theme(legend.position="none")
  }

  return(p)
}
