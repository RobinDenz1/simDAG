
## A node to generate arbitrary time-to-event data in
## discrete-time simulations
#' @importFrom data.table fifelse
#' @export
node_time_to_event <- function(data, parents, sim_time, name, prob_fun,
                               prob_fun_args=list(), event_duration=0,
                               immunity_duration=event_duration,
                               save_past_events=TRUE, check_inputs=TRUE,
                               envir) {

  if (check_inputs) {
    check_inputs_node_time_to_event(data=data, parents=parents,
                                    sim_time=sim_time, name=name,
                                    prob_fun=prob_fun,
                                    prob_fun_args=prob_fun_args,
                                    event_duration=event_duration,
                                    immunity_duration=immunity_duration,
                                    save_past_events=save_past_events)
  }

  # get list of arguments
  prob_fun_args$data <- data

  if ("sim_time" %in% names(formals(prob_fun))) {
    prob_fun_args$sim_time <- sim_time
  }

  # get event probabilities
  event_prob <- do.call(prob_fun, args=prob_fun_args)

  # specific names
  name_event <- paste0(name, "_event")
  name_time <- paste0(name, "_time")
  name_past_event_times <- paste0(name, "_past_event_times")

  days_since_event <- sim_time - data[[name_time]]

  # set event_prob to 1 if event is ongoing and to 0 if it is no longer
  # ongoing but the person is still immune, keep estimated probability
  # otherwise
  event_prob <- fifelse(is.na(data[[name_time]]), event_prob,
                        fifelse(data[[name_event]] &
                                days_since_event < event_duration, 1,
                        fifelse(data[[name_event]] &
                                days_since_event < immunity_duration, 0,
                                event_prob)))

  # draw new events based on this probability
  event <- rbernoulli(n=nrow(data), p=event_prob)

  # update event time
  event_time <- fifelse(is.na(data[[name_time]]) & event, sim_time,
                        fifelse(is.na(data[[name_time]]) &
                                !event, NA_integer_,
                        fifelse(!is.na(data[[name_time]]) &
                                days_since_event < immunity_duration,
                                data[[name_time]], NA_integer_)))

  # update past event times
  # NOTE: Looks weird, but is super efficient because the list does not have
  #       to be copied and the assignment is based on a single index, meaning
  #       no searching, looping etc.
  if (save_past_events) {

    # ids with a new event at this point in time
    ids_new_event <- data$.id[!is.na(event_time) & event_time==sim_time]

    if (!is.null(ids_new_event)) {
      # assign id vector to environment of sim_discrete_time function
      assign(x="ids_new_event", value=ids_new_event, envir=envir)

      # this vector is then assigned to the respective list
      assign2list(name="past_events_list",
                  i=name_past_event_times,
                  j=sim_time,
                  value="ids_new_event",
                  envir=envir)
    }
  }

  # put together
  out <- data.table::data.table(event=event,
                                event_time=event_time)
  colnames(out) <- c(name_event, name_time)

  return(out)
}

## special assign function which assigns a vector of person ids to a list
## storing previous events for all time_to_event nodes
# NOTE: i is a string specifying which variable, j is the time index
assign2list <- function(name, i, j, value, envir){
  paste0(name, "[['", i, "']][[", j, "]] <- ", value) |>
    str2lang() |>
    eval(envir=envir)
}
