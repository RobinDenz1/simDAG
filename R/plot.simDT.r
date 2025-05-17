
## S3 plot method for simDT objects, creating a flowchart for the simulation
## design
#' @importFrom rlang .data
#' @export
plot.simDT <- function(x, right_boxes=TRUE,
                       box_hdist=1, box_vdist=1,
                       box_l_width=0.35, box_l_height=0.23,
                       box_r_width=box_l_width,
                       box_r_height=box_l_height + 0.1,
                       box_alpha=0.5, box_linetype="solid",
                       box_linewidth=0.5, box_border_colors=NULL,
                       box_fill_colors=NULL, box_text_color="black",
                       box_text_alpha=1, box_text_angle=0,
                       box_text_family="sans", box_text_fontface="plain",
                       box_text_size=5, box_text_lineheight=1,
                       box_1_text_left="Create initial data",
                       box_1_text_right=NULL, box_2_text="Increase t by 1",
                       box_l_node_labels=NULL, box_r_node_labels=NULL,
                       box_last_text=paste0("t <= ", x$max_t, "?"),
                       arrow_line_type="solid", arrow_line_width=0.5,
                       arrow_line_color="black", arrow_line_alpha=1,
                       arrow_head_angle=30, arrow_head_size=0.3,
                       arrow_head_unit="cm", arrow_head_type="closed",
                       arrow_left_pad=0.3, hline_width=0.5,
                       hline_type="dashed", hline_color="black",
                       hline_alpha=1, ...) {

  requireNamespace("ggplot2", quietly=TRUE)

  if (!inherits(x, "simDT")) {
    stop("'x' must be a simDT object created using the sim_discrete_time()",
         " function.", call.=FALSE)
  }

  tx_names <- unlist(lapply(x$tx_nodes, FUN=function(x){x$name}))

  check_inputs_plot.simDT(right_boxes=right_boxes,
                          box_hdist=box_hdist,
                          box_vdist=box_vdist,
                          box_l_width=box_l_width,
                          box_l_height=box_l_height,
                          box_r_width=box_r_width,
                          box_r_height=box_r_height,
                          box_1_text_left=box_1_text_left,
                          box_1_text_right=box_1_text_right,
                          box_2_text=box_2_text,
                          box_l_node_labels=box_l_node_labels,
                          box_r_node_labels=box_r_node_labels,
                          box_last_text=box_last_text,
                          tx_names=tx_names)

  # default labels
  if (is.null(box_l_node_labels)) {
    box_l_node_labels <- get_left_box_labels(tx_names)
  }
  if (is.null(box_r_node_labels)) {
    box_r_node_labels <- get_right_box_labels(tx_names=tx_names,
                                              tx_nodes=x$tx_nodes)
  }
  if (is.null(box_1_text_right)) {
    box_1_text_right <- paste0("Includes:\n'",
                               paste(x$t0_var_names, collapse="', '"), "'")
  }

  # needed data for all parts of the plot
  d_box_left <- get_left_box_data(node_labels=box_l_node_labels,
                                  box_width=box_l_width,
                                  box_height=box_l_height,
                                  box_vdist=box_vdist,
                                  box_hdist=box_hdist,
                                  box_1_text_left=box_1_text_left,
                                  box_2_text=box_2_text,
                                  box_last_text=box_last_text)

  d_box_right <- get_right_box_data(data=d_box_left,
                                    box_width=box_r_width,
                                    box_height=box_r_height,
                                    box_hdist=box_hdist,
                                    node_labels=box_r_node_labels,
                                    box_1_text_right=box_1_text_right,
                                    box_2_text=box_2_text,
                                    box_last_text=box_last_text)

  d_down_arrows <- get_down_arrow_data(d_box_left)

  d_hlines <- get_horizontal_line_data(box_data=d_box_left,
                                       box_hdist=box_hdist,
                                       box_2_text=box_2_text,
                                       box_last_text=box_last_text)

  d_t_arrow_lines <- get_t_plus_1_line_data(box_data=d_box_left,
                                            box_vdist=box_vdist,
                                            box_height=box_l_height,
                                            box_width=box_l_width,
                                            arrow_left_pad=arrow_left_pad,
                                            box_2_text=box_2_text)

  d_t_arrow_head <- get_t_plus_1_arrow_data(box_data=d_box_left,
                                            box_width=box_l_width,
                                            arrow_left_pad=arrow_left_pad,
                                            box_2_text=box_2_text)

  # create left boxes
  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data=d_box_left,
      mapping=ggplot2::aes(xmin=.data$xmin,
                           ymin=.data$ymin,
                           xmax=.data$xmax,
                           ymax=.data$ymax,
                           fill=.data$type,
                           colour=.data$type),
      alpha=box_alpha,
      linetype=box_linetype,
      linewidth=box_linewidth
  )

  # add labels for boxes on the left
  p <- p + ggplot2::geom_text(
    data=d_box_left,
    mapping=ggplot2::aes(x=.data$x, y=.data$y, label=.data$label),
    colour=box_text_color,
    alpha=box_text_alpha,
    size=box_text_size,
    lineheight=box_text_lineheight,
    angle=box_text_angle,
    family=box_text_family,
    fontface=box_text_fontface
  )

  if (right_boxes) {

    # add boxes on the right
    p <- p + ggplot2::geom_rect(
      data=d_box_right,
      mapping=ggplot2::aes(xmin=.data$xmin,
                           ymin=.data$ymin,
                           xmax=.data$xmax,
                           ymax=.data$ymax,
                           fill=.data$type,
                           colour=.data$type),
      alpha=box_alpha,
      linetype=box_linetype,
      linewidth=box_linewidth
    )

    # add labels for boxes on the right
    p <- p + ggplot2::geom_text(
      data=d_box_right,
      mapping=ggplot2::aes(x=.data$x, y=.data$y, label=.data$label),
      colour=box_text_color,
      alpha=box_text_alpha,
      size=box_text_size,
      lineheight=box_text_lineheight,
      angle=box_text_angle,
      family=box_text_family,
      fontface=box_text_fontface
    )
  }

  # custom box colors
  if (!is.null(box_border_colors)) {
    p <- p + ggplot2::scale_colour_manual(values=box_border_colors)
  }
  if (!is.null(box_fill_colors)) {
    p <- p + ggplot2::scale_fill_manual(values=box_fill_colors)
  }

  # add arrows from top to bottom
  p <- p + ggplot2::geom_segment(
    data=d_down_arrows,
    ggplot2::aes(x=.data$x, y=.data$y, xend=.data$xend, yend=.data$yend),
    arrow=ggplot2::arrow(length=ggplot2::unit(arrow_head_size,
                                             arrow_head_unit),
                         type=arrow_head_type,
                         angle=arrow_head_angle),
    linetype=arrow_line_type,
    color=arrow_line_color,
    linewidth=arrow_line_width,
    alpha=arrow_line_alpha
  )

  if (right_boxes) {
    # add lines from left to right
    p <- p + ggplot2::geom_segment(
      data=d_hlines,
      ggplot2::aes(x=.data$x, y=.data$y, xend=.data$xend, yend=.data$yend),
      linetype=hline_type,
      colour=hline_color,
      linewidth=hline_width,
      alpha=hline_alpha
    )
  }

  # add line for arrow that goes back to t + 1 box
  p <- p + ggplot2::geom_segment(
    data=d_t_arrow_lines,
    ggplot2::aes(x=.data$x, y=.data$y, xend=.data$xend, yend=.data$yend),
    linetype=arrow_line_type,
    color=arrow_line_color,
    linewidth=arrow_line_width,
    alpha=arrow_line_alpha
  )

  # add arrow for increase t by 1
  p <- p + ggplot2::geom_segment(
    data=d_t_arrow_head,
    ggplot2::aes(x=.data$x, y=.data$y, xend=.data$xend, yend=.data$yend),
    arrow=ggplot2::arrow(length=ggplot2::unit(arrow_head_size,
                                              arrow_head_unit),
                         type=arrow_head_type,
                         angle=arrow_head_angle),
    linetype=arrow_line_type,
    color=arrow_line_color,
    linewidth=arrow_line_width,
    alpha=arrow_line_alpha
  )

  # remove usual ggplot2 stuff
  p <- p + ggplot2::theme_void() +
    ggplot2::theme(legend.position="none")

  return(p)
}
