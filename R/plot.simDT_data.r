
## initialize the data which creates the boxes on the left side
get_left_box_data <- function(node_labels, box_width, box_height, box_vdist,
                              box_hdist, box_1_text_left, box_2_text,
                              box_last_text) {

  # main line with nodes
  data <- data.frame(x=0,
                     y=rev(seq_len(length(node_labels) + 3)),
                     label=c(box_1_text_left, box_2_text,
                             node_labels, box_last_text),
                     type=c("none", "none",
                            rep("time_varying_node", length(node_labels)),
                            "none"))

  data$y <- data$y * box_vdist
  data <- add_box_coordinates(data, box_width, box_height)

  return(data)
}

## initialize data for the boxes on the right side
get_right_box_data <- function(data, box_width, box_height, box_hdist,
                               node_labels, box_1_text_right, box_2_text,
                               box_last_text) {

  data$x <- data$x + box_hdist
  data <- add_box_coordinates(data, box_width, box_height)
  data <- data[data$label!=box_2_text & data$label!=box_last_text, ]
  data$label <- c(box_1_text_right, node_labels)

  return(data)
}

## given x, y, box_width and box_height, adds the 4 required points to
## plot boxes using geom_rect() to the data argument
add_box_coordinates <- function(data, box_width, box_height) {

  x <- y <- NULL

  data <- within(data, {
    xmin <- x - box_width
    xmax <- x + box_width
    ymin <- y - box_height
    ymax <- y + box_height
  })

  return(data)
}

## generate labels for the boxes on the left side
get_left_box_labels <- function(tx_names) {

  labels <- paste0("Generate '", tx_names, "' status")

  return(labels)
}

## generate labels for the boxes on the right side
get_right_box_labels <- function(tx_names, tx_nodes) {

  # labels for each tx_node
  labels <- character(length=length(tx_nodes))
  for (i in seq_len(length(tx_nodes))) {

    if (tx_nodes[[i]]$type=="time_to_event" |
        tx_nodes[[i]]$type=="competing_events") {
      label_i <- paste0("Probability dependent on:\n'",
                        paste(tx_nodes[[i]]$parents, collapse="', '"), "'")
    } else {
      label_i <- paste0("Change dependent on:\n'",
                        paste(tx_nodes[[i]]$parents, collapse="', '"), "'")
    }

    labels[i] <- label_i
  }

  return(labels)
}

## initialize data needed to add the directed arrows between the boxes
get_down_arrow_data <- function(box_data) {

  x <- ymin <- y <- ymax <- NULL

  box_data <- within(box_data,{
    xend <- x
    y <- ymin
    yend <- data.table::shift(ymax, type="lead")
  })

  box_data <- box_data[!is.na(box_data$yend), ]

  return(box_data)
}

## initialize data for horizontal lines between boxes
get_horizontal_line_data <- function(box_data, box_hdist, box_2_text,
                                     box_last_text) {
  y <- xmax <- xmin <- NULL

  box_data <- box_data[box_data$label!=box_2_text &
                       box_data$label!=box_last_text, ]

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
                                   box_width, arrow_left_pad, box_2_text) {

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
                      yend=box_data$y[box_data$label==box_2_text])

  data <- rbind(line1, line2, line3)

  return(data)
}

## initialize data for the arrow that goes back to the
## increase t by 1 box
get_t_plus_1_arrow_data <- function(box_data, box_width, arrow_left_pad,
                                    box_2_text) {
  x <- xmin <- y <- NULL

  box_data <- box_data[box_data$label==box_2_text, ]

  box_data <- within(box_data, {
    x <- -box_width - arrow_left_pad
    xend <- xmin
    yend <- y
  })

  return(box_data)
}
