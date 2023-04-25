
## transform output from the sim_discrete_time function into the
## start-stop format
#' @export
sim2start_stop <- function(sim, include_tx_nodes=FALSE,
                           use_save_states=TRUE) {

  if (sim$save_states=="all") {
    data <- sim2start_stop.all(sim=sim)
  } else if (sim$save_states=="last") {
    data <- sim2start_stop.last(sim=sim,
                                include_tx_nodes=include_tx_nodes)
  } else if (sim$save_states=="at_t") {
    stop("This function currently does not work if save_states='at_t'",
         " was used in the original sim_discrete_time() call.")
  }

  return(data)
}

## used when save_states="all" was used in sim_discrete_time
sim2start_stop.all <- function(sim) {

  # transform to long format
  data <- sim2long(sim=sim)

  varying <- unlist(lapply(sim$tx_nodes, FUN=function(x){x$name}))
  data <- long2start_stop(data=data, id=".id", time=".simulation_time",
                          varying=varying)

  return(data)
}

## takes the output of the sim_discrete_time function called with
## save_states="last" and outputs a data.table in the start / stop format
#' @importFrom data.table fifelse
#' @importFrom data.table data.table
#' @importFrom data.table setkey
#' @importFrom data.table merge.data.table
#' @importFrom data.table setcolorder
#' @importFrom data.table dcast
#' @importFrom data.table :=
sim2start_stop.last <- function(sim, include_tx_nodes) {

  start <- .id <- NULL

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

  rm(inv_tte_past_events, tte_n_events)

  # get needed vectors
  vec_all_events <- unlist(tte_all)

  # if no events at all happened, stop here
  if (is.null(vec_all_events)) {
    data <- data.table(.id=seq_len(n_sim), start=0, stop=max_t)
    data[, (tte_names) := FALSE]
    return(data)
  }

  vec_all_n <- unlist(tte_n)
  vec_event_durations <- rep(rep(event_durations, n_sim), vec_all_n)
  vec_all_events_end <- vec_all_events + vec_event_durations
  vec_id <- rep(rep(seq_len(n_sim), each=length(tte_names)), vec_all_n)
  vec_kind <- rep(rep(tte_names, n_sim), vec_all_n)

  rm(tte_all, tte_n)

  # initial data.table
  data <- data.table(.id=rep(vec_id, 2),
                     start=c(vec_all_events, vec_all_events_end))

  # add zeros
  start_rows <- data.table(.id=1:n_sim, start=0)
  data <- rbind(data, start_rows)

  # remove invalidly long times
  data <- data[start <= max_t, ]

  # sort by .id and start
  setkey(data, .id, start)

  # create stop
  data[, stop := shift(start, type="lead", fill=max_t + 1), by=.id]

  # initialize table storing all events + durations + kind
  events_dat <- data.table(.id=vec_id,
                           start=vec_all_events,
                           end=vec_all_events_end,
                           kind=vec_kind)
  setkey(events_dat, .id, start)

  # create one end for each event
  events_dat <- dcast(events_dat, .id + start ~ kind, value.var="end")

  rm(vec_all_events, vec_all_n, vec_event_durations,
     vec_all_events_end, vec_id, vec_kind)

  data <- merge.data.table(data, events_dat, by=c(".id", "start"),
                           all.x=TRUE, all.y=FALSE)

  rm(events_dat)

  # fill up ends & create event indicators
  for (i in seq_len(length(tte_names))) {
    name <- tte_names[i]

    if (name %in% colnames(data)) {
      data[, (name) := na_locf(eval(parse(text=name))), by=.id]
      data[, (name) := !is.na(eval(parse(text=name))) &
             start < eval(parse(text=name))]
    } else {
      data[, (name) := FALSE]
    }
  }

  # extract other variables
  if (include_tx_nodes) {
    remove_vars <- c(paste0(tte_names, "_event"),
                     paste0(tte_names, "_time"))
  } else {
    remove_vars <- c(paste0(tte_names, "_event"),
                     paste0(tte_names, "_time"),
                     non_tte_names)
  }

  data_t0 <- sim$data[, !remove_vars, with=FALSE]

  # merge with start / stop data
  data <- data[data_t0, on=".id"]

  # correct end of intervals
  data[, stop := stop - 1]
  data <- data[start <= stop & !duplicated(data), ]

  # reorder columns
  first_cols <- c(".id", "start", "stop", tte_names)
  setcolorder(data, c(first_cols,
                      colnames(data)[!colnames(data) %in% first_cols]))

  setkey(data, NULL)

  return(data)
}

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

## last observation carried forward
na_locf <- function(x) {
  v <- !is.na(x)
  return(c(NA, x[v])[cumsum(v) + 1])
}
