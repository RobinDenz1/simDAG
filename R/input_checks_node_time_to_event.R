
## checking the inputs of the node_time_to_event function
check_inputs_node_time_to_event <- function(data, parents, sim_time, name,
                                            prob_fun, prob_fun_args,
                                            event_duration, immunity_duration,
                                            save_past_events) {
  # rudimentary type checks
  stopifnot("'data' must be a data.frame." = is.data.frame(data))
  stopifnot("'parents' must be a vector of type character." =
              is.vector(parents, mode = "character"))
  stopifnot("'sim_time' must be a single integer." =
              (length(sim_time) == 1 && is.numeric(sim_time)))
  stopifnot("'name' must be a single character." =
              (length(name) == 1 && is.character(name)))
  stopifnot("'prob_fun' must be a function or a single number." =
              is.function(prob_fun) | (is.numeric(prob_fun) &
                                       length(prob_fun)==1))
  stopifnot("'event_duration' must be a single integer." =
              (length(event_duration) == 1 && is.numeric(event_duration)))
  stopifnot("'immunity_duration' must be a single integer." =
              (length(immunity_duration) == 1 &&
                 is.numeric(immunity_duration)))
  stopifnot("'save_past_events' must be either TRUE or FALSE." =
              (length(save_past_events) == 1 && is.logical(save_past_events)))

  # check content of data
  if (is.data.frame(data)) {
    stopifnot(
      "'data' needs to include at least one variable." =
                (ncol(data) != 0))
    stopifnot(
      "'data' needs to include at least one row." =
      (nrow(data) != 0))
  }

  # check content of prob_fun
  if (is.function(prob_fun)) {
    stopifnot(
      "'prob_fun' needs to include at least one line." =
                length(body(prob_fun)) != 0)

    # check content of prob_fun_args
    if (length(setdiff(names(formals(prob_fun)),
                       c("data", "sim_time"))) != 0) {
      for (i in 1:length(setdiff(names(formals(prob_fun)),
                                 c("data", "sim_time")))) {
        if(!is.element(setdiff(names(formals(prob_fun)),
                              c("data", "sim_time"))[i],
                      names(prob_fun_args))) {
          stop("All parameters of 'prob_fun' except 'data' and 'sim_time'",
               " must be included in the node_td() call if they don't have a",
               " default value.")
        }
      }
    }
  }
}
