
## perform a discrete time simulation based on
## previously defined functions and nodes
# TODO:
#   - this function desperately needs tests!
#   - also needs documentation + examples
#   - also needs (probably a lot of) input checks
#' @export
sim_discrete_time <- function(n_sim=NULL, t0_root_nodes=NULL,
                              t0_child_nodes=NULL, t0_sort_dag=TRUE,
                              t0_data=NULL,
                              t0_transform_fun=NULL,
                              t0_transform_args=list(), max_t,
                              tx_nodes, tx_nodes_order=NULL,
                              tx_transform_fun=NULL,
                              tx_transform_args=list(),
                              save_states="last", save_states_at=NULL,
                              filename=NULL) {
  # get initial data
  if (is.null(t0_data)) {
    data <- sim_from_dag(n_sim=n_sim,
                         root_nodes=t0_root_nodes,
                         child_nodes=t0_child_nodes,
                         sort_dag=t0_sort_dag)
  } else {
    data <- as.data.frame(t0_data)
  }
  data$id <- seq(1, nrow(data))

  # perform an arbitrary data transformation right at the start
  if (!is.null(t0_transform_fun)) {
    args$data <- data
    data <- do.call(t0_transform_fun, args=t0_transform_args)
  }

  # initialize list for saving past states of the simulation
  if (save_states=="last") {
    past_states <- NULL
  } else if (save_states=="all") {
    past_states <- vector(mode="list", length=max_t)
  } else if (save_states=="at_t" & !is.null(save_states_at)) {
    past_states <- vector(mode="list", length=length(save_states_at))
    state_count <- 1
  }

  # use custom order of execution if specified,
  # otherwise just loop over node list
  if (is.null(tx_nodes_order)) {
    tx_nodes_order <- seq_len(length(tx_nodes))
  }

  # get relevant node names
  tx_node_names <- vapply(tx_nodes, function(x){x$name},
                          FUN.VALUE=character(1))
  tx_node_types <- vapply(tx_nodes, function(x){x$type},
                          FUN.VALUE=character(1))
  tte_names <- apply(expand.grid(tx_node_names[tx_node_types=="time_to_event"],
                                 c("event", "time", "past_event_times")), 1,
                     paste, collapse="_")

  # add missing columns to data
  init_colnames <- c(tx_node_names[tx_node_types!="time_to_event"], tte_names)
  existing_colnames <- colnames(data)
  for (i in seq_len(length(init_colnames))) {
    if (!init_colnames[i] %in% existing_colnames) {
      if (endsWith(init_colnames[i], "_event")) {
        data[, init_colnames[i]] <- FALSE
      } else {
        data[, init_colnames[i]] <- NA
      }
    }
  }

  # start the main loop
  for (t in seq_len(max_t)) {

    # execute each node function one by one
    for (i in tx_nodes_order) {
      # get relevant arguments
      args <- tx_nodes[[i]]
      args$data <- data
      args$type <- NULL

      # get function
      node_type_fun <- get(paste0("node_", tx_nodes[[i]]$type))
      fun_pos_args <- names(formals(node_type_fun))

      # add or remove internal arguments if needed
      if ("sim_time" %in% fun_pos_args) {
        args$sim_time <- t
      }
      if (!"name" %in% fun_pos_args) {
        args$name <- NULL
      }
      if (!"parents" %in% fun_pos_args) {
        args$parents <- NULL
      }

      # call needed node function
      node_out <- do.call(node_type_fun, args)
      data <- add_node_to_data(data=data, new=node_out,
                               name=tx_nodes[[i]]$name)
    }

    # perform an arbitrary data transformation after each time point
    if (!is.null(tx_transform_fun)) {
      args$data <- data
      data <- do.call(tx_transform_fun, args=tx_transform_args)
    }

    # save intermediate simulation states, if specified
    if (save_states=="all") {
      data$simulation_time <- t
      past_states[[t]] <- data
    } else if (save_states=="at_t" & t %in% save_states_at) {
      data$simulation_time <- t
      past_states[[state_count]] <- data
      state_count <- state_count + 1
    }
  }

  out <- list(past_states=past_states,
              data=data)

  if (!is.null(filename)) {
    saveRDS(out, filename)
  }

  return(out)
}
