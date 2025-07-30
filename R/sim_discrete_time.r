
## perform a discrete time simulation based on
## previously defined functions and nodes
#' @export
sim_discrete_time <- function(dag, n_sim=NULL, t0_sort_dag=FALSE,
                              t0_data=NULL, t0_transform_fun=NULL,
                              t0_transform_args=list(), max_t,
                              tx_nodes_order=NULL,
                              tx_transform_fun=NULL, tx_transform_args=list(),
                              save_states="last", save_states_at=NULL,
                              save_networks=FALSE,
                              verbose=FALSE, check_inputs=TRUE) {

  if (!inherits(dag, "DAG")) {
    stop("'dag' must be a DAG object created using the empty_dag() and",
         " node_td() functions.", call.=FALSE)
  }
  tx_nodes <- dag$tx_nodes

  requireNamespace("data.table", quietly=TRUE)

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
    t0_transform_args$data <- data
    data <- do.call(t0_transform_fun, args=t0_transform_args)
  }

  # initialize list for saving past states of the simulation
  if (save_states=="last") {
    past_states <- past_networks <- NULL
  } else if (save_states=="all") {
    past_states <- past_networks <- vector(mode="list", length=max_t)
  } else if (save_states=="at_t" & !is.null(save_states_at)) {
    past_states <- past_networks <- vector(mode="list",
                                           length=length(save_states_at))
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
  tx_node_types <- vapply(tx_nodes, function(x){x$type_str},
                          FUN.VALUE=character(1))

  data <- initialize_columns(data=data,
                             tx_nodes=tx_nodes,
                             tx_node_names=tx_node_names,
                             tx_node_types=tx_node_types)

  # define a list of arguments once so it doesn't have to be changed
  # inside the double for loop
  arg_list <- lapply(tx_nodes, clean_node_args)
  fun_list <- lapply(tx_nodes, FUN=function(x){x$type_fun})

  # indicator if time-dependent networks are present
  has_td_networks <- length(vapply(dag$networks,
                                   FUN=function(x){x$time_varying},
                                   FUN.VALUE=logical(1))) > 0

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

    # update networks, if needed
    if (has_td_networks) {
      dag$networks <- create_networks(networks=dag$networks,
                                      n_sim=n_sim,
                                      data=data,
                                      sim_time=t,
                                      past_states=past_states)
    }

    # execute each node function one by one
    for (i in tx_nodes_order) {

      if (verbose) {
        cat("t = ", t, " node = ", tx_nodes[[i]]$name, "\n", sep="")
      }

      # get relevant arguments
      args <- arg_list[[i]]

      if (!is.null(tx_nodes[[i]]$formula) &&
          !is_formula(tx_nodes[[i]]$formula)) {

        # augment data for formula input
        args$data <- tryCatch({
          data_for_formula(data=data, args=args, networks=dag$networks)},
          error=function(e){
            stop("An error occured when interpreting the formula of node '",
                 tx_nodes[[i]]$name, "'. The message was:\n", e,
                 call.=FALSE)
          }
        )
      } else {
        args$data <- data[, args$parents, with=FALSE]
      }

      # remove temporary mixed model stuff if there
      if (!is.null(args$mixed_terms)) {
        args$mixed_terms <- NULL
      }

      # get function
      node_type_fun <- fun_list[[i]]
      fun_pos_args <- names(formals(node_type_fun))

      # add or remove internal arguments if needed
      if ("sim_time" %in% fun_pos_args) {
        args$sim_time <- t
      }
      if ("past_states" %in% fun_pos_args) {
        args$past_states <- past_states
      }
      if (!"parents" %in% fun_pos_args) {
        args$parents <- NULL
      }

      if (tx_node_types[i]=="time_to_event" |
          tx_node_types[i]=="competing_events") {
        args$envir <- envir
      } else {
        # only done for nodes of other types to allow this argument
        # in prob_fun()
        if (!"intercept" %in% fun_pos_args) {
          args$intercept <- NULL
        }
      }

      # call needed node function and make possible errors more
      # informative by adding node and time
      node_out <- tryCatch({
        do.call(node_type_fun, args)},
        error=function(e){
          stop("An error occured when processing node '", tx_nodes[[i]]$name,
               "' at time t = ", t, ". The message was:\n", e, call.=FALSE)
        }
      )

      # add output to data, also make possible error messages more
      # informative here
      data <- tryCatch({
        add_node_to_data(data=data, new=node_out, name=tx_nodes[[i]]$name)},
        error=function(e){
          stop("An error occured when trying to add the output of node '",
               tx_nodes[[i]]$name, "' at time t = ", t, " to the current",
               " data. The message was:\n", e, call.=FALSE)
        }
      )
    }

    # perform an arbitrary data transformation after each time point
    if (!is.null(tx_transform_fun)) {
      tx_transform_args$data <- data
      data <- tryCatch({
        do.call(tx_transform_fun, args=tx_transform_args)},
        error=function(e){
          stop("An error occured when calling the tx_transform() function",
               " at t = ", t, ". The message was:\n", e, call.=FALSE)
        }
      )
    }

    # save intermediate simulation states, if specified
    if (save_states=="all") {
      past_states[[t]] <- data

      if (save_networks) {
        past_networks[[t]] <- dag$networks
      }
    } else if (save_states=="at_t" & t %in% save_states_at) {
      past_states[[state_count]] <- data

      if (save_networks) {
        past_networks[[state_count]] <- dag$networks
      }
      state_count <- state_count + 1
    }
  }

  out <- list(past_states=past_states,
              past_networks=past_networks,
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
