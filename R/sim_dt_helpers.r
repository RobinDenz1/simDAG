
## add output of a node function to data.frame
add_node_to_data <- function(data, new, name) {
  if (is.data.frame(new)) {
    new_names <- colnames(new)
    for (i in seq_len(length(new_names))) {
      data[[new_names[i]]] <- new[[new_names[i]]]
    }
  } else {
    data[, name] <- new
  }
  return(data)
}

## add some parent nodes that should be passed automatically
add_missing_parents <- function(node) {

  # change parents arguments if time_to_event node
  if (node$type=="time_to_event" | node$type=="competing_events") {
    parents <- c(".id", node$parents,
                 paste0(node$name, c("_event", "_time")))
  } else {
    parents <- node$parents
  }

  # add optional columns if time-to-event node
  if (node$type=="time_to_event" && !is.null(node$time_since_last) &&
      node$time_since_last) {
    parents <- c(parents, paste0(node$name, "_time_since_last"))
  }

  if (node$type=="time_to_event" && !is.null(node$event_count) &&
      node$event_count) {
    parents <- c(parents, paste0(node$name, "_event_count"))
  }

  node$parents <- unique(parents)

  return(node)
}

## small function to get a valid list of node arguments
clean_node_args <- function(node) {

  # get function
  node_type_fun <- get(paste0("node_", node$type))
  fun_pos_args <- names(formals(node_type_fun))

  node <- add_missing_parents(node)

  # add or remove internal arguments if needed
  if (!"name" %in% fun_pos_args) {
    node$name <- NULL
  }
  node$type <- NULL
  node$time_varying <- NULL

  return(node)
}

## create an empty list of list with the right dimensions
## one element each for every tte_node containing max_t empty spaces each
setup_past_events_list <- function(names, max_t) {

  if (length(names)==0) {
    out <- list()
  } else {
    out <- vector(mode="list", length=length(names))
    names(out) <- names
    for (i in seq_len(length(out))) {
      out[[i]] <- vector(mode="list", length=max_t)
    }
  }
  return(out)
}

## generate column names for each time-to-event node included in data
get_tte_names <- function(tx_node_names, tx_node_types, tx_nodes) {

  tte_names <- c()
  for (i in seq_len(length(tx_node_names))) {
    if (tx_node_types[i] == "time_to_event") {
      tte_names[length(tte_names) + 1] <- paste0(tx_node_names[i], "_event")
      tte_names[length(tte_names) + 1] <- paste0(tx_node_names[i], "_time")

      event_count <- tx_nodes[[i]]$event_count
      time_since_last <- tx_nodes[[i]]$time_since_last

      if (!is.null(event_count) && event_count) {
        tte_names[length(tte_names) + 1] <- paste0(tx_node_names[i],
                                                   "_event_count")
      }

      if (!is.null(time_since_last) && time_since_last) {
        tte_names[length(tte_names) + 1] <- paste0(tx_node_names[i],
                                                   "_time_since_last")
      }
    }
  }

  return(tte_names)
}

## generate column names for each competing-events node included in data
get_ce_names <- function(tx_node_names, tx_node_types) {

  ce_names <- apply(expand.grid(
    tx_node_names[tx_node_types=="competing_events"], c("event", "time")), 1,
    paste, collapse="_")

  if (length(ce_names)==0) {
    ce_names <- NULL
  }

  return(ce_names)
}

## check if a node has an argument and optionally if that argument is
## set to TRUE
node_has_arg <- function(node, arg, arg_is_true=FALSE) {

  if (is.null(node$parents) && !node$time_varying) {
    arg <- node$params[[arg]]
  } else {
    arg <- node[[arg]]
  }

  if (!is.null(arg)) {
    if (arg_is_true && is.logical(arg) && all(arg)) {
      out <- TRUE
    } else if (arg_is_true) {
      out <- FALSE
    } else {
      out <- TRUE
    }
  } else {
    out <- FALSE
  }
  return(out)
}

## add columns that will be needed for time-to-event and
## competing event nodes
initialize_columns <- function(data, tx_nodes, tx_node_names, tx_node_types) {

  tte_names <- get_tte_names(tx_node_names=tx_node_names,
                             tx_node_types=tx_node_types,
                             tx_nodes=tx_nodes)
  ce_names <- get_ce_names(tx_node_names=tx_node_names,
                           tx_node_types=tx_node_types)

  # add missing columns to data
  init_colnames <- c(tx_node_names[tx_node_types!="time_to_event" &
                                   tx_node_types!="competing_events"],
                     tte_names, ce_names)
  existing_colnames <- colnames(data)
  for (i in seq_len(length(init_colnames))) {
    if (!init_colnames[i] %in% existing_colnames) {
      if (endsWith(init_colnames[i], "_event")) {
        data[, init_colnames[i]] <- FALSE
      } else if (endsWith(init_colnames[i], "_event_count")) {
        data[, init_colnames[i]] <- 0
      } else {
        data[, init_colnames[i]] <- NA_integer_
      }
    }
  }

  return(data)
}
