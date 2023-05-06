
## initialize the data which creates the boxes needed in the plot
get_box_data <- function(tx_names, box_width, box_height, box_vdist,
                         box_hdist, increase_t_text="Increase t by 1",
                         max_t_text) {

  # main line with nodes
  data <- data.frame(x=0,
                     y=rev(seq_len(length(tx_names)+3)),
                     label=c("Create initial data", increase_t_text,
                             tx_names, max_t_text),
                     type=c("none", "none",
                            rep("time_varying_node", length(tx_names)),
                            "none"))

  data$y <- data$y * box_vdist
  data <- add_box_coordinates(data, box_width, box_height)

  # boxes to the right of it (including some additional info)
  data_right <- data
  data_right$x <- data_right$x + box_hdist
  data_right <- add_box_coordinates(data_right, box_width, box_height)
  data_right <- data_right[data_right$label!=increase_t_text &
                           data_right$label!=max_t_text, ]

  # put together
  data <- rbind(data, data_right)

  return(data)
}

## given x, y, box_width and box_height, adds the 4 required points to
## plot boxes using geom_rect() to the data argument
add_box_coordinates <- function(data, box_width, box_height) {

  x <- y <- NULL

  data <- within(data, {
    xmin <- x - box_width;
    xmax <- x + box_width;
    ymin <- y - box_height;
    ymax <- y + box_height;
  })

  return(data)
}

## initialize data needed to add the directed arrows between the boxes
get_down_arrow_data <- function(box_data) {

  x <- ymin <- y <- ymax <- NULL

  box_data <- box_data[box_data$x==0, ]

  box_data <- within(box_data,{
    xend <- x
    y <- ymin
    yend <- data.table::shift(ymax, type="lead")
  })

  box_data <- box_data[!is.na(box_data$yend), ]

  return(box_data)
}

## initialize data for horizontal lines between boxes
get_horizontal_line_data <- function(box_data, box_hdist, increase_t_text,
                                     max_t_text) {
  y <- xmax <- xmin <- NULL

  box_data <- box_data[box_data$x==0 & box_data$label!=increase_t_text &
                       box_data$label!=max_t_text, ]

  box_data <- within(box_data, {
    yend <- y
    x <- xmax
    xend <- xmin + box_hdist
  })

  return(box_data)
}

## initialize data for the lines in the arrow that goes back to the
## increase t by 1 box
get_t_plus_1_line_data <- function(box_data, box_vdist, box_height,
                                   box_width, arrow_left_pad, increase_t_text) {

  box_data <- box_data[box_data$x==0, ]

  # from last box down
  line1 <- data.frame(x=0,
                      xend=0,
                      y=min(box_data$ymin),
                      yend=min(box_data$y) - (box_vdist - box_height))

  # from line1 end to left
  line2 <- data.frame(x=0,
                      xend=-box_width - arrow_left_pad,
                      y=min(box_data$ymax) - box_vdist,
                      yend=min(box_data$ymax) - box_vdist)

  # from line2 end up
  line3 <- data.frame(x=-box_width - arrow_left_pad,
                      xend=-box_width - arrow_left_pad,
                      y=min(box_data$ymax) - box_vdist,
                      yend=box_data$y[box_data$label==increase_t_text])

  data <- rbind(line1, line2, line3)

  return(data)
}

## initialize data for the arrow that goes back to the
## increase t by 1 box
get_t_plus_1_arrow_data <- function(box_data, box_width, arrow_left_pad,
                                    increase_t_text) {
  x <- xmin <- y <- NULL

  box_data <- box_data[box_data$label==increase_t_text, ]

  box_data <- within(box_data, {
    x <- -box_width - arrow_left_pad
    xend <- xmin
    yend <- y
  })

  return(box_data)
}

## S3 plot method for simDT objects, creating a flowchart for the simulation
## design
#' @importFrom rlang .data
#' @export
plot.simDT <- function(x,
                       increase_t_text="Increase t by 1",
                       max_t_text=paste0("t <= ", x$max_t, "?"),
                       box_width=0.35,
                       box_height=0.23,
                       box_hdist=1,
                       box_vdist=1,
                       box_alpha=0.5,
                       box_linetype="solid",
                       box_linewidth=0.5,
                       box_border_colors=NULL,
                       box_fill_colors=NULL,
                       box_text_color="black",
                       box_text_alpha=1,
                       box_text_angle=0,
                       box_text_family="sans",
                       box_text_fontface="plain",
                       box_text_size=5,
                       box_text_lineheight=1,
                       arrow_line_type="solid",
                       arrow_line_width=0.5,
                       arrow_line_color="black",
                       arrow_line_alpha=1,
                       arrow_head_angle=30,
                       arrow_head_size=0.3,
                       arrow_head_unit="cm",
                       arrow_head_type="closed",
                       arrow_left_pad=0.3,
                       hline_width=0.5,
                       hline_type="dashed",
                       hline_color="black",
                       hline_alpha=1,
                       ...) {

  requireNamespace("ggplot2")

  tx_names <- unlist(lapply(x$tx_nodes, FUN=function(x){x$name}))

  # needed data for all parts of the plot
  d_box <- get_box_data(tx_names=tx_names,
                        box_width=box_width,
                        box_height=box_height,
                        box_vdist=box_vdist,
                        box_hdist=box_hdist,
                        increase_t_text=increase_t_text,
                        max_t_text=max_t_text)

  d_down_arrows <- get_down_arrow_data(d_box)

  d_hlines <- get_horizontal_line_data(box_data=d_box,
                                       box_hdist=box_hdist,
                                       increase_t_text=increase_t_text,
                                       max_t_text=max_t_text)

  d_t_arrow_lines <- get_t_plus_1_line_data(box_data=d_box,
                                            box_vdist=box_vdist,
                                            box_height=box_height,
                                            box_width=box_width,
                                            arrow_left_pad=arrow_left_pad,
                                            increase_t_text=increase_t_text)

  d_t_arrow_head <- get_t_plus_1_arrow_data(box_data=d_box,
                                            box_width=box_width,
                                            arrow_left_pad=arrow_left_pad,
                                            increase_t_text=increase_t_text)

  # create boxes
  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data=d_box,
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

  # custom box colors
  if (!is.null(box_border_colors)) {
    p <- p + ggplot2::scale_colour_manual(values=box_border_colors)
  }
  if (!is.null(box_fill_colors)) {
    p <- p + ggplot2::scale_fill_manual(values=box_fill_colors)
  }

  # add labels
  p <- p + ggplot2::geom_text(
    data=d_box,
    mapping=ggplot2::aes(x=.data$x, y=.data$y, label=.data$label),
    colour=box_text_color,
    alpha=box_text_alpha,
    size=box_text_size,
    lineheight=box_text_lineheight,
    angle=box_text_angle,
    family=box_text_family,
    fontface=box_text_fontface
  )

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

  # add lines from left to right
  p <- p + ggplot2::geom_segment(
    data=d_hlines,
    ggplot2::aes(x=.data$x, y=.data$y, xend=.data$xend, yend=.data$yend),
    linetype=hline_type,
    colour=hline_color,
    linewidth=hline_width,
    alpha=hline_alpha
  )

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
