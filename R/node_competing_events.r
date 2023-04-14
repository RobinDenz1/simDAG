
## A node to generate arbitrary time-to-event data with competing events in
## discrete-time simulations
#' @importFrom data.table fifelse
#' @export
node_competing_events <- function(data, parents, sim_time, name, prob_fun,
                                  prob_fun_args=list(), event_duration=c(0, 0),
                                  immunity_duration=max(event_duration),
                                  save_past_events=TRUE, envir) {
  # get list of arguments
  prob_fun_args$data <- data

  if ("sim_time" %in% names(formals(prob_fun))) {
    prob_fun_args$sim_time <- sim_time
  }

  # get event probabilities
  event_prob <- do.call(prob_fun, args=prob_fun_args)

  # draw new events based on this probability
  event <- rcategorical(n=nrow(data), probs=event_prob)

  # specific names
  name_event <- paste0(name, "_event")
  name_time <- paste0(name, "_time")
  name_past_event_times <- paste0(name, "_past_event_times")
  name_past_event_causes <- paste0(name, "_past_event_causes")

  days_since_event <- sim_time - data[[name_time]]

  # stuff to pick the right event
  event_duration <- c(-1, event_duration)
  names(event_duration) <- seq(0, (ncol(event_prob)-1))
  event_chr <- as.character(data[[name_event]])

  # rows where the probability should be one
  ids <- seq_len(nrow(data))
  ids_1 <- ids[!is.na(data[[name_time]]) &
               (days_since_event <= event_duration[event_chr])]

  # rows where the probability should be zero
  ids_0 <- ids[!is.na(data[[name_time]]) &
               (days_since_event <= immunity_duration) &
               !days_since_event <= event_duration[event_chr]]

  # set to past event if ongoing
  event[ids_1] <- data[[name_event]][ids_1]

  # set to no event if immune
  event[ids_0] <- 0

  # update event time
  event_time <- fifelse(is.na(data[[name_time]]) & event!=0, sim_time,
                        fifelse(is.na(data[[name_time]]) &
                                event==0, NA_integer_,
                        fifelse(!is.na(data[[name_time]]) &
                                days_since_event <= immunity_duration,
                                data[[name_time]], NA_integer_)))

  # update past event times and kinds, see node_time_to_event function
  if (save_past_events) {
    ## times
    cond <- (data[[name_event]] != event) & event!=0
    ids_new_event <- data$.id[cond]

    if (!is.null(ids_new_event)) {
      # assign id vector to environment of sim_discrete_time function
      assign(x="ids_new_comp_event", value=ids_new_event, envir=envir)

      # this vector is then assigned to the respective list
      assign2list(name="past_comp_events_list",
                  i=name_past_event_times,
                  j=sim_time,
                  value="ids_new_comp_event",
                  envir=envir)

      ## causes
      new_comp_causes <- event[cond]
      assign(x="new_comp_causes", value=new_comp_causes, envir=envir)
      assign2list(name="past_comp_causes_list",
                  i=name_past_event_causes,
                  j=sim_time,
                  value="new_comp_causes",
                  envir=envir)
    }
  }

  # put together
  out <- data.table::data.table(event=event,
                                event_time=event_time)
  colnames(out) <- c(name_event, name_time)

  return(out)
}
