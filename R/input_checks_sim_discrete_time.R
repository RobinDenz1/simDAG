## checking the inputs of the sim_discrete_time function
check_inputs_sim_discrete_time <- function(n_sim, t0_root_nodes,
                                           t0_child_nodes, t0_sort_dag,
                                           t0_data,t0_transform_fun,
                                           t0_transform_args,max_t,
                                           tx_nodes, tx_nodes_order,
                                           tx_transform_fun, tx_transform_args,
                                           save_states, save_states_at,
                                           verbose) {
  # rudimentary type checks
  if (!is.null(t0_data)) {
    stopifnot("'t0_data' must be a data.frame." = is.data.frame(t0_data))
  } else {
    ## rudimentary type checks for variables 'n_sim', 't0_root_nodes',
    ## 't0_child_nodes' and 't0_sort_dag' in
    ## function 'check_inputs_sim_from_dag'
    stopifnot("''n_sim' must be a single integer > 0." = !is.null(n_sim))
    stopifnot("'t0_root_nodes' must be either a data.frame or a list." =
                !is.null(t0_root_nodes))
    stopifnot("'t0_child_nodes' must be a list." =
                !is.null(t0_child_nodes))
    stopifnot("'t0_sort_dag' must be either TRUE or FALSE." =
                !is.null(t0_sort_dag))
  }
  if (!is.null(t0_transform_fun)) {
    stopifnot("'t0_transform_fun' must be a function." =
                is.function(t0_transform_fun))
  }
  stopifnot("'t0_transform_args' must be a list." = is.list(t0_transform_args))
  stopifnot("'max_t' must be a single integer." =
              (length(max_t) == 1 && is.numeric(max_t)))
  stopifnot("'tx_nodes' must be a list." = is.list(tx_nodes))
  if (!is.null(tx_nodes_order)) {
    stopifnot("'tx_nodes_order' must be a vector of type numeric." =
                is.vector(tx_nodes_order, mode = "numeric"))
  }
  if (!is.null(tx_transform_fun)) {
    stopifnot("'tx_transform_fun' must be a function." =
                is.function(tx_transform_fun))
  }
  stopifnot("'tx_transform_args' must be a list." =
              is.list(tx_transform_args))
  stopifnot("'save_states' must be a single character." =
              (length(save_states) == 1 && is.character(save_states)))
  if (!is.null(save_states_at)) {
    if (!(length(save_states_at) == 1 && is.numeric(save_states_at)) ||
         !(is.vector(save_states_at, mode = "numeric")))  {
      stop("'save_states_at' must be either a single integer",
           " or a vector of type numeric.")
    }
  }
  stopifnot("'verbose' must be logical." = is.logical(verbose))

  # check content of t0_data
  if (is.data.frame(t0_data)) {
    stopifnot("'t0_data' needs to include at least one variable." =
                  (ncol(t0_data) != 0))
    stopifnot("'t0_data' needs to include at least one row." =
                (nrow(t0_data) != 0))
  }

  # check content of t0_transform_fun
  if (is.function(t0_transform_fun)) {
    stopifnot(
      "'t0_transform_fun' needs to include at least one line." =
        length(body(t0_transform_fun)) != 0)

    # check content of t0_transform_args
    if (length(names(formals(t0_transform_fun))) == 0) {
      if (!identical(length(names(formals(t0_transform_fun))),
          length(names(t0_transform_args)))) {
        stop("Defined parameters in 't0_transform_args' are not used",
             " for 't0_transform_fun'.")
      }
    } else {
      for (i in 1:length(names(formals(t0_transform_fun)))) {
        if (!is.element(names(formals(t0_transform_fun))[i],
                        names(t0_transform_args))) {
          stop("All parameters of 't0_transform_fun' must be defined",
               " in 't0_transform_args'.")
        }
      }
    }
  }

  # check content of tx_nodes
  if (is.list(tx_nodes)) {
    for (i in 1:length(tx_nodes)) {
      stopifnot("All elements of 'tx_nodes' must have a name." =
                  (length(tx_nodes[[i]]$name) == 1 &&
                     is.character(tx_nodes[[i]]$name)))
      if (!is.null(tx_nodes[[i]]$parents)) {
        stopifnot(
          "All elements of 'tx_nodes' must have at least one parent." =
                    (length(tx_nodes[[i]]$parents >= 1) &&
                       is.vector(tx_nodes[[i]]$parents, mode = "character")))
      }
      stopifnot("All elements of 'tx_nodes' must have a type." =
                  (length(tx_nodes[[i]]$type) == 1 &&
                     is.character(tx_nodes[[i]]$type)))
      ## rudimentary type checks for variables 'prob_fun', 'prob_fun_args',
      ##'event_duration', 'immunity_duration' and 'save_past_events'
      ##'in function 'check_inputs_node_time_to_event'
      if (tx_nodes[[i]]$type == "time_to_event") {
        stopifnot(
          "Elements of type 'time_to_event' must have a prob_fun." =
            !is.null(tx_nodes[[i]]$prob_fun))
      ## variables 'prob_fun_args', 'event_duration', 'immunity_duration'
      ## and 'save_past_events' have default values
      }
    }
  }

  # check content of tx_nodes_order
  if (is.vector(tx_nodes_order)) {
    if (!identical(length(tx_nodes_order), length(tx_nodes))) {
      stop("'tx_nodes_order' must be the same length as",
           " the number of nodes of 'tx_nodes.")
    }
    if (!all(is.element(tx_nodes_order, seq_len(length(tx_nodes))))) {
      stop("'tx_nodes_order' must contain the same elements as",
           " elements in 'tx_nodes'.")
    }
  }

  # check content of tx_transform_fun
  if (is.function(tx_transform_fun)) {
    stopifnot("'tx_transform_fun' needs to include at least one line." =
                length(body(tx_transform_fun)) != 0)

    # check content of tx_transform_args
    if (length(names(formals(tx_transform_fun))) == 0) {
      if (!identical(length(names(formals(tx_transform_fun))),
                     length(names(tx_transform_args)))) {
        stop("Defined parameters in 'tx_transform_args' are not used",
             " for 'tx_transform_fun'.")
      }
    } else {
      for(i in 1:length(names(formals(tx_transform_fun)))) {
        if (!is.element(names(formals(tx_transform_fun))[i],
                        names(tx_transform_args))) {
          stop("All parameters of 'tx_transform_fun' must be defined",
               " in 'tx_transform_args'.")
        }
      }
    }
  }

  # check content of save_states
  if (is.character(save_states)) {
    stopifnot("'save_states' must be either 'last', 'all' or 'at_t'." =
                is.element(save_states, c("last", "all", "at_t")))
  }
}
