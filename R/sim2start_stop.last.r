
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

## identify if a certain period should be considered an event or not
which_events <- function(sim_time, .id, inv_tte_past_events, event_durations) {

  out <- logical(length(event_durations))

  for (i in seq_len(length(event_durations))) {

    events_start <- inv_tte_past_events[[i]][[.id]]

    if (!is.null(events_start)) {
      events_end <- events_start + event_durations[i]

      if (any(sim_time >= events_start & sim_time < events_end)) {
        out[i] <- TRUE
      }
    }
  }
  return(out)
}

## vectorized version of which_events to be used in apply() call
identify_events <- function(x, inv_tte_past_events, event_durations) {
  which_events(x["start"], x[".id"], inv_tte_past_events, event_durations)
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

  # get needed vectors
  vec_all_events <- unlist(tte_all)
  vec_all_n <- unlist(tte_n)
  vec_event_durations <- rep(rep(event_durations, n_sim), vec_all_n)
  vec_all_events_end <- vec_all_events + vec_event_durations
  vec_id <- rep(rep(1:n_sim, each=length(tte_names)), vec_all_n)

  # initial data.table
  data <- data.table(.id=rep(vec_id, 2),
                     start=c(vec_all_events, vec_all_events_end))

  # add zeros
  start_rows <- data.table(.id=1:n_sim,
                           start=0)
  data <- rbind(data, start_rows)

  # remove invalidly long times
  data <- data[start <= max_t, ]

  # sort by .id and start
  setkey(data, .id, start)

  # create stop
  data <- data[, stop := shift(start, type="lead", fill=max_t), by=.id]

  # initialize table storing all events + durations + kind
  events_dat <- data.table(.id=vec_id,
                           start=vec_all_events,
                           end=vec_all_events_end)

  # get event indicators
  # NOTE: this is the computationally most expensive part, a more clever
  #       vectorized way to do this could result in a big speedup
  event_ind <- apply(data, MARGIN=1, FUN=identify_events,
                     event_durations=event_durations,
                     inv_tte_past_events=inv_tte_past_events)

  # add event indicators to data
  for (i in seq_len(length(tte_names))) {
    data[[tte_names[i]]] <- event_ind[i,]
  }

  data <- data[data$start!=max_t & data$start!=data$stop, ]

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
  data <- data[data_t0, on=".id"]

  # how to code the intervals
  if (interval=="stop_minus_1") {
    data$stop[data$stop < max_t] <- data$stop[data$stop < max_t] - 1
  } else if (interval=="start_plus_1") {
    data$start[data$start > 0] <- data$start[data$start > 0] + 1
  }

  return(data)
}
