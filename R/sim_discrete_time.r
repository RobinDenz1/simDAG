
## perform a discrete time simulation based on
## previously defined functions and nodes
#' @export
sim_discrete_time <- function(dag, n_sim=NULL, t0_sort_dag=TRUE,
                              t0_data=NULL, t0_transform_fun=NULL,
                              t0_transform_args=list(), max_t,
                              tx_nodes_order=NULL,
                              tx_transform_fun=NULL, tx_transform_args=list(),
                              save_states="last", save_states_at=NULL,
                              verbose=FALSE, check_inputs=TRUE) {

  if (!inherits(dag, "DAG")) {
    stop("'dag' must be a DAG object created using the empty_dag() and",
         " node() functions.")
  }
  tx_nodes <- dag$tx_nodes

  if (check_inputs) {
    check_inputs_sim_discrete_time(n_sim=n_sim, dag=dag,
                                   t0_sort_dag=t0_sort_dag, t0_data=t0_data,
                                   t0_transform_fun=t0_transform_fun,
                                   t0_transform_args=t0_transform_args,
                                   max_t=max_t, tx_nodes=tx_nodes,
                                   tx_nodes_order=tx_nodes_order,
                                   tx_transform_fun=tx_transform_fun,
                                   tx_transform_args=tx_transform_args,
                                   save_states=save_states,
                                   save_states_at=save_states_at,
                                   verbose=verbose)
  }

  .id <- NULL

  # get initial data
  if (is.null(t0_data) & length(dag$root_nodes)==0 &
      length(dag$child_nodes)==0) {
    data <- data.table(.id=seq(1, n_sim))
  } else if (is.null(t0_data)) {
    dag$tx_nodes <- NULL
    data <- sim_from_dag(n_sim=n_sim,
                         dag=dag,
                         sort_dag=t0_sort_dag,
                         check_inputs=check_inputs)
    data.table::setDT(data)
  } else {
    data <- data.table::setDT(t0_data)
  }
  t0_var_names <- colnames(data)

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

  data <- initialize_columns(data=data,
                             tx_nodes=tx_nodes,
                             tx_node_names=tx_node_names,
                             tx_node_types=tx_node_types)

  # define a list of arguments once so it doesn't have to be changed
  # inside the double for loop
  arg_list <- lapply(tx_nodes, clean_node_args)
  fun_list <- lapply(tx_nodes, FUN=function(x){get(paste0("node_", x$type))})

  # get current environment
  envir <- environment()

  # setup lists for storing the past events of all time_to_event nodes
  # time_to_event nodes
  past_events_list <- setup_past_events_list(names=tx_node_names[
    tx_node_types=="time_to_event"], max_t=max_t)

  # competing_events nodes
  past_comp_events_list <- setup_past_events_list(names=tx_node_names[
    tx_node_types=="competing_events"], max_t=max_t)

  past_comp_causes_list <- past_comp_events_list

  # create and assign id
  data[, .id := seq(1, nrow(data))]

  # start the main loop
  for (t in seq_len(max_t)) {
    # execute each node function one by one
    for (i in tx_nodes_order) {

      if (verbose) {
        cat("t = ", t, " node = ", tx_nodes[[i]]$name, "\n", sep="")
      }

      # get relevant arguments
      args <- arg_list[[i]]
      args$data <- data[, args$parents, with=FALSE]

      # get function
      node_type_fun <- fun_list[[i]]
      fun_pos_args <- names(formals(node_type_fun))

      # add or remove internal arguments if needed
      if ("sim_time" %in% fun_pos_args) {
        args$sim_time <- t
      }
      if (!"parents" %in% fun_pos_args) {
        args$parents <- NULL
      }
      if (tx_node_types[i]=="time_to_event" |
          tx_node_types[i]=="competing_events") {
        args$envir <- envir
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
      past_states[[t]] <- data
    } else if (save_states=="at_t" & t %in% save_states_at) {
      past_states[[state_count]] <- data
      state_count <- state_count + 1
    }
  }

  out <- list(past_states=past_states,
              save_states=save_states,
              data=data,
              tte_past_events=past_events_list,
              ce_past_events=past_comp_events_list,
              ce_past_causes=past_comp_causes_list,
              tx_nodes=tx_nodes,
              max_t=max_t,
              t0_var_names=t0_var_names)
  class(out) <- "simDT"

  return(out)
}

## S3 print method for sim_discrete_time output
#' @export
print.simDT <- function(x, ...) {
  cat("A simDT object with:\n")
  cat("  - ", nrow(x$data), " observations\n")
  cat("  - ", x$max_t, " distinct points in time\n")
  cat("  - ", length(x$tx_nodes), " time-varying variables in total\n")
  cat("  - ", length(x$tte_past_events), " time_to_event nodes\n")
  cat("  - ", length(x$ce_past_events), " competing_events nodes\n")

  if (x$save_states=="all") {
    output_str <- "States of the simulation were saved at all points in time."
  } else if (x$save_states=="last") {
    output_str <- "Only the last state of the simulation was saved."
  } else if (x$save_states=="at_t") {
    output_str <- paste0("States of the simulation were saved at user-",
                         "defined points in time.")
  }

  cat(output_str, "\n")
}

## S3 summary method for sim_discrete_time output
#' @export
summary.simDT <- function(object, ...) {
  print.simDT(x=object, ...)
}
