## checking the inputs of the sim_discrete_time function
check_inputs_sim_discrete_time <- function(n_sim, t0_root_nodes, t0_child_nodes,
                                           t0_sort_dag, t0_data, t0_transform_fun,
                                           t0_transform_args, max_t, tx_nodes,
                                           tx_nodes_order, tx_transform_fun,
                                           tx_transform_args, save_states,
                                           save_states_at, verbose) {
  # rudimentary type checks
  if (!is.null(t0_data)) {
    stopifnot("'t0_data' must be a data.frame." = is.data.frame(t0_data))
  } else {
    ## rudimentary type checks for variables 'n_sim', 't0_root_nodes',
    ## 't0_child_nodes' and 't0_sort_dag' in function 'check_inputs_sim_from_dag'
    stopifnot("''n_sim' must be a single integer > 0." = !is.null(n_sim))
    stopifnot("'root_nodes' must be either a data.frame or a list." =
                !is.null(t0_root_nodes))
    stopifnot("'child_nodes' must be a list." = !is.null(t0_child_nodes))
    stopifnot("'sort_dag' must be either TRUE or FALSE." = !is.null(t0_sort_dag))
  }
  if (!is.null(t0_transform_fun)) {
    stopifnot("'t0_transform_fun' must be a function." = is.function(prob_fun))
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
    stopifnot("'tx_transform_fun' must be a function." = is.function(tx_transform_fun))
  }
  if (!is.null(tx_transform_args)) {
    stopifnot("'tx_transform_args' must be a list." = is.list(tx_transform_args))
  }
  stopifnot("'save_states' must be a single character." =
              (length(save_states) == 1 && is.character(save_states)))
  if (!is.null(save_states_at)) {
    stopifnot("'save_states_at' must be either a single integer or a vector of type numeric." =
                ((length(save_states_at) == 1 && is.numeric(save_states_at)) ||
                   is.vector(save_states_at, mode = "numeric")))
  }
  stopifnot("'verbose' must be logical." = is.logical(verbose))

  # check content of t0_data
  if (is.data.frame(t0_data)) {
    stopifnot("If a data.frame is supplied to 't0_data' it needs to include at least one variable." =
                (ncol(t0_data) != 0))
    stopifnot("If a data.frame is supplied to 't0_data' it needs to include at least one row.") =
      (nrow(t0_data) != 0)
  }

  # check content of t0_transform_fun
  if (is.function(t0_transform_fun)) {
    stopifnot("If a function is supplied to 't0_transform_fun' it needs to include at least one line." =
                length(body(t0_transform_fun)) != 0)

    # check content of t0_transform_args
    if (is.list(t0_transform_args)) {
      for (i in 1:length(names(formals(t0_transform_fun)))) {
        stopchar <- paste("Variable '", names(formals(t0_transform_fun))[i],
                          "' must be defined in 't0_transform_args' for 't0_transform_fun'.",
                          sep = "")
        stopifnot(stopchar = is.element(names(formals(t0_transform_fun))[i],
                                        names(t0_transform_args)))
      }
    }
  }

  # check content of tx_nodes
  if (is.list(tx_nodes)) {
    for (i in 1:length(tx_nodes)) {
      # check name of nodes
      stopchar1 <- paste("Element", i,
                         "must have a name in form of a single character.",
                         sep = "")
      stopifnot(stopchar1 = (length(tx_nodes[[i]]$name) == 1 && is.character(tx_nodes[[i]]$name)))

      # check parents of nodes
      if (!is.null(tx_nodes[[i]]$parents)) {
        stopchar2 <- paste("Element", i,
                           "must have at least one parent in form of a single character or a vector of type character.",
                           sep = "")
        stopifnot(stopchar2 = (length((tx_nodes[[i]]$parents >= 1 &&
                                         is.vector(tx_nodes[[i]]$parents, mode = "character")))))
      }

      # check type of nodes
      stopchar3 <- paste("Element", i,
                         "must have a type in form of a single character.",
                         sep = "")
      stopifnot(stopchar3 = (length(tx_nodes[[i]]$type) == 1 && is.character(tx_nodes[[i]]$type)))

      ## rudimentary type checks for variables 'prob_fun', 'prob_fun_args',
      ## 'event_duration', 'immunity_duration' and 'save_past_events'
      ## in function 'check_inputs_node_time_to_event'
      if (tx_nodes[[i]]$type == "time_to_event") {
        # check prob_fun of nodes
        stopchar4 <- paste("Element", i,
                           "must have a prob_fun in form of a function.",
                           sep = "")
        stopifnot(stopchar4 = !is.null(tx_nodes[[i]]$prob_fun))

        # check prob_fun_args of nodes
        stopchar5 <- paste("Element", i,
                           "must have a prob_fun_args in form of a list.",
                           sep = "")
        stopifnot(stopchar5 = !is.null(tx_nodes[[i]]$prob_fun_args))

        # check event_duration of nodes
        stopchar6 <- paste("Element", i,
                           "must have a event_duration in form of a single integer.",
                           sep = "")
        stopifnot(stopchar6 = !is.null(tx_nodes[[i]]$event_duration))

        # check immunity_duration of nodes
        stopchar6 <- paste("Element", i,
                           "must have a immunity_duration in form of a single integer.",
                           sep = "")
        stopifnot(stopchar6 = !is.null(tx_nodes[[i]]$immunity_duration))

        # check save_past_event of nodes
        stopchar7 <- paste("Element", i,
                           "must have defined whether past events should be saved or not.",
                           sep = "")
        stopifnot(stopchar7 = !is.null(tx_nodes[[i]]$save_past_events))
      }
    }
  }

  # check content of tx_nodes_order
  if (is.vector(tx_nodes_order)) {
    stopifnot("If a vector is supplied to 'tx_nodes_order' it must be the same length as the number of nodes of 'tx_nodes." =
                identical(length(tx_nodes_order), length(tx_nodes)))
    stopifnot("If a vector is supplied to 'tx_nodes_order' it must contain the same elements as elements in 'tx_nodes'." =
                is.element(tx_nodes_order, seq_len(length(tx_nodes))))
  }

  # check content of tx_transform_fun
  if (is.function(tx_transform_fun)) {
    stopifnot("If a function is supplied to 'tx_transform_fun' it needs to include at least one line." =
                length(body(tx_transform_fun)) != 0)

    # check content of tx_transform_args
    if (is.list(tx_transform_args)) {
      for(i in 1:length(names(formals(tx_transform_fun)))) {
        stopchar <- paste("Variable '", names(formals(tx_transform_fun))[i],
                          "' must be defined in 'tx_transform_args' for 'tx_transform_fun'.",
                          sep = "")
        stopifnot(stopchar = is.element(names(formals(tx_transform_fun))[i],
                                        names(tx_transform_args)))
      }
    }
  }

  # check content of save_states
  if (is.character(save_states)) {
    stopifnot("'save_states' must be either 'last', 'all' or 'at_t'." =
                is.element(save_past, c("last", "all", "at_t")))
  }
}
