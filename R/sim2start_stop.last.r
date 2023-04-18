
## takes a list with one entry per time point filled with person ids
## and outputs a list with one entry per person with time ids
invert_event_time_list <- function(tte_list, n_sim) {

  out <- vector(mode="list", length=n_sim)

  for (i in seq_len(length(tte_list))) {
    for (j in seq_len(length(tte_list[[i]]))) {
      person_id <- tte_list[[i]][[j]]
      out[[person_id]] <- append(out[[person_id]], i)
    }
  }
  return(out)
}

## takes a list of list where each list on the inside has the same length
## and is filled with vectors or NULL, returns a new list with the same
## length as the inside list containing concatenated vectors
merge_nested_lists <- function(nested_list) {

  n_sim <- length(nested_list[[1]])

  out <- vector(mode="list", length=n_sim)

  for (i in seq_len(length(nested_list))) {
    for (j in seq_len(n_sim)) {
      # do nothing if NULL
      if (is.null(nested_list[[i]][[j]])) {

      # simply assign it if first in line
      } else if (i == 1) {
        out[[j]] <- nested_list[[i]][[j]]
      # append it to existing vector otherwise
      } else {
        out[[j]] <- append(out[[j]], nested_list[[i]][[j]])
      }
    }
  }
  return(out)
}

## checks if an event is still ongoing at time t given
## a vector of all start and stop times of all events of a person
is_ongoing_event <- function(sim_time, events_start, events_end) {
  any(sim_time > events_start & sim_time <= events_end)
}

## creates start / stop data for a single person
vec2start_stop <- function(.id, events, event_kinds, event_durations,
                           tte_names, max_t) {

  # calculate time at end of each event
  events_end <- events + event_durations

  # sort all possible event times
  sorted_events <- sort(c(events, events_end))
  sorted_events <- sorted_events[sorted_events <= max_t]

  # initialize data.table with start and stop times
  rows <- data.table::data.table(.id=.id,
                                 start=c(0, sorted_events),
                                 stop=c(sorted_events, max_t))

  # add correct status indicator to each period
  for (i in seq_len(length(tte_names))) {
    rows[[tte_names[i]]] <- vapply(
      rows$stop,
      FUN=is_ongoing_event,
      FUN.VALUE=logical(1),
      events_start=events[event_kinds==tte_names[i]],
      events_end=events_end[event_kinds==tte_names[i]]
    )
  }
  return(rows)
}

## given a node list, returns the used event duration
## if not specified by user, returns default value
get_event_duration <- function(node) {

  if (is.null(node$event_duration)) {
    dur <- formals(node_time_to_event)$event_duration
  } else {
    dur <- node$event_duration
  }
  return(dur)
}

## takes the output of the sim_discrete_time function called with
## save_states="last" and outputs a data.table in the start / stop format
# NOTE: mostly memory efficient, but not optimized for speed yet
sim2start_stop.last <- function(sim, include_tx_nodes, interval) {

  n_sim <- nrow(sim$data)
  max_t <- sim$max_t

  # extract past events
  tte_past_events <- sim$tte_past_events

  # get names of time-varying variables
  tx_names <- unlist(lapply(sim$tx_nodes, FUN=function(x){x$name}))
  tx_type <- unlist(lapply(sim$tx_nodes, FUN=function(x){x$type}))
  tte_names <- tx_names[tx_type=="time_to_event"]
  non_tte_names <- tx_names[tx_type!="time_to_event"]

  # get durations of each time-to-event node
  event_durations <- vapply(sim$tx_nodes[tx_type=="time_to_event"],
                            FUN=get_event_duration,
                            FUN.VALUE=numeric(1))

  # create list of inverted tte_lists and a list containing the number
  # of events per person per event kind
  inv_tte_past_events <- vector(mode="list", length=length(tte_past_events))
  tte_n_events <- vector(mode="list", length(tte_past_events))

  for (i in seq_len(length(tte_past_events))) {
    inv_tte_past_events[[i]] <- invert_event_time_list(tte_past_events[[i]],
                                                       n_sim=n_sim)
    tte_n_events[[i]] <- vapply(inv_tte_past_events[[i]],
                                FUN=length,
                                FUN.VALUE=integer(1))
  }

  # merge them into one list for further processing
  tte_all <- merge_nested_lists(inv_tte_past_events)
  tte_n <- merge_nested_lists(tte_n_events)

  # create start / stop data for each .id
  # NOTE: this is the only computationally expensive part, might need
  #       some adjustments or a different approach
  out <- vector(mode="list", length=n_sim)
  for (i in seq_len(n_sim)) {

    out[[i]] <- vec2start_stop(.id=i,
                               events=tte_all[[i]],
                               event_durations=rep(event_durations, tte_n[[i]]),
                               event_kinds=rep(tte_names, tte_n[[i]]),
                               tte_names=tte_names,
                               max_t=max_t)
  }
  out <- data.table::rbindlist(out)

  out <- out[out$start!=max_t, ]

  # extract other variables
  if (include_tx_nodes) {
    remove_vars <- c(paste0(tte_names, "_event"),
                     paste0(tte_names, "_time"),
                     ".simulation_time")
  } else {
    remove_vars <- c(paste0(tte_names, "_event"),
                     paste0(tte_names, "_time"),
                     non_tte_names, ".simulation_time")
  }

  data_t0 <- sim$data[, !remove_vars, with=FALSE]

  # merge with start / stop data
  out <- out[data_t0, on=".id"]

  # how to code the intervals
  if (interval=="stop_minus_1") {
    out$stop[out$stop < max_t] <- out$stop[out$stop < max_t] - 1
  } else if (interval=="start_plus_1") {
    out$start[out$start > 0] <- out$start[out$start > 0] + 1
  }

  return(out)
}
