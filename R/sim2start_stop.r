
## transform output from the sim_discrete_time function into the
## start-stop format
sim2start_stop <- function(sim, use_saved_states=sim$save_states=="all",
                           overlap=FALSE, target_event=NULL,
                           keep_only_first=FALSE, remove_not_at_risk=FALSE) {

  if (use_saved_states) {
    data <- sim2start_stop.all(sim=sim, overlap=overlap,
                               target_event=target_event,
                               keep_only_first=keep_only_first,
                               remove_not_at_risk=remove_not_at_risk)
  } else {
    data <- sim2start_stop.last(sim=sim, overlap=overlap,
                                target_event=target_event,
                                keep_only_first=keep_only_first,
                                remove_not_at_risk=remove_not_at_risk)
  }

  return(data)
}

## used when save_states="all" was used in sim_discrete_time
sim2start_stop.all <- function(sim, overlap=FALSE, target_event=NULL,
                               keep_only_first=FALSE,
                               remove_not_at_risk=FALSE) {

  .id <- .event_count <- .event_cumsum <- NULL

  # transform to long format
  data <- sim2long(sim=sim)

  # remove optional time-to-event columns
  c_names <- colnames(data)
  c_names <- c_names[!(endsWith(c_names, "_time_since_last") |
                       endsWith(c_names, "_event_count"))]
  data <- data[, c_names, with=FALSE]

  # names of time-varying nodes
  varying <- unlist(lapply(sim$tx_nodes, FUN=function(x){x$name}))

  # prepare long-format if outcome specific data.table is desired
  if (!is.null(target_event)) {
    event_duration <-
      get_event_duration(sim$tx_nodes[varying==target_event][[1]])

    # set event to FALSE after initial occurrence to get
    # length 1 event intervals
    if (event_duration > 1) {
      data[, .event_cumsum := cumsum(eval(parse(text=target_event))), by=.id]
      data[, (target_event) := (.event_cumsum %% event_duration)==1]
      data[, .event_cumsum := NULL]
    }

    # create event count to make sure that subsequent, distinct events
    # are not grouped together
    if (event_duration == 1) {
      data[, .event_count := cumsum(eval(parse(text=target_event))), by=.id]
      varying <- c(varying, ".event_count")
    }
  }

  # transform to start-stop format
  data <- long2start_stop(data=data, id=".id", time=".time",
                          varying=varying, overlap=overlap,
                          check_inputs=FALSE)

  # make it outcome centric
  if (!is.null(target_event)) {

    if (event_duration==1) {
      data[, .event_count := NULL]
    }
    data <- collapse_for_target_event(data, target_event=target_event,
                                      keep_only_first=keep_only_first)

    # remove time not at-risk after event onset if specified
    if (remove_not_at_risk) {
      target_duration <-
        get_event_duration(node=sim$tx_nodes[varying==target_event][[1]],
                           type="immunity_duration")

      data <- remove_not_at_risk(data=data, duration=target_duration,
                                 target_event=target_event,
                                 overlap=overlap)
    }
  }
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
sim2start_stop.last <- function(sim, overlap=FALSE, target_event=NULL,
                                keep_only_first=FALSE,
                                remove_not_at_risk=FALSE) {

  # temporary error message
  if (length(sim$ce_past_events) > 0) {
    stop("This function currently does not work if competing_events",
         " nodes were used in the original sim_discrete_time() call",
         " in conjunction with setting save_states!='all'.", call.=FALSE)
  }

  start <- .id <- NULL

  n_sim <- nrow(sim$data)
  max_t <- sim$max_t

  # extract past events
  tte_past_events <- sim$tte_past_events

  # get names of time-varying variables
  tx_names <- unlist(lapply(sim$tx_nodes, FUN=function(x){x$name}))
  tx_type <- unlist(lapply(sim$tx_nodes, FUN=function(x){x$type_str}))
  tte_names <- tx_names[tx_type=="time_to_event"]
  non_tte_names <- tx_names[tx_type!="time_to_event"]

  # without any tte nodes, simply return the last state of data
  if (length(tte_names) == 0) {
    data <- sim$data
    data[, start := 1]
    data[, stop := max_t]

    first_cols <- c(".id", "start", "stop")
    setcolorder(data, c(first_cols,
                        colnames(data)[!colnames(data) %in% first_cols]))
    setkey(data, NULL)

    return(data)
  }

  # get durations of each time-to-event node
  event_durations <- vapply(sim$tx_nodes[tx_type=="time_to_event"],
                            FUN=get_event_duration,
                            FUN.VALUE=numeric(1))

  # artificially set duration of target event to 1
  if (!is.null(target_event)) {
    event_durations[tte_names==target_event] <- 1
  }

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
    data <- data.table(.id=seq_len(n_sim), start=1, stop=(max_t + 1))
    data[, (tte_names) := FALSE]
  } else {

    vec_all_n <- unlist(tte_n)
    vec_event_durations <- rep(rep(event_durations, n_sim), vec_all_n)
    vec_all_events_end <- vec_all_events + vec_event_durations
    vec_id <- rep(rep(seq_len(n_sim), each=length(tte_names)), vec_all_n)
    vec_kind <- rep(rep(tte_names, n_sim), vec_all_n)

    rm(tte_all, tte_n)

    # initial data.table
    data <- data.table(.id=rep(vec_id, 2),
                       start=c(vec_all_events, vec_all_events_end))

    # add first time
    start_rows <- data.table(.id=1:n_sim, start=1)
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
  }

  # extract other variables
  remove_vars <- c(paste0(tte_names, "_event"),
                   paste0(tte_names, "_time"),
                   paste0(tte_names, "_time_since_last"),
                   paste0(tte_names, "_event_count"))

  data_t0 <- suppressWarnings(sim$data[, !remove_vars, with=FALSE])

  # merge with start / stop data
  data <- data[data_t0, on=".id"]

  # correct end of intervals
  data[, stop := stop - 1]
  data <- data[start <= stop & !duplicated(data), ]

  # if specified, make it event centric
  if (!is.null(target_event)) {
    data <- collapse_for_target_event(data, target_event=target_event,
                                      keep_only_first=keep_only_first)

    # remove time after event onset where people are not at risk if
    # specified by user
    if (remove_not_at_risk) {

      target_duration <- get_event_duration(
        node=sim$tx_nodes[tx_names==target_event][[1]],
        type="immunity_duration"
      )
      data <- remove_not_at_risk(data=data, duration=target_duration,
                                 target_event=target_event, overlap=FALSE)
    }
  }

  # if intervals should be overlapping, add them back
  if (overlap) {
    data[, stop := stop + 1]
  }

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

## given a node list, returns the used event_duration or immunity_duration
## if not specified by user, returns default value
get_event_duration <- function(node, type="event_duration") {

  in_node <- node[[type]]

  if (is.null(in_node) && type=="immunity_duration") {
    in_node <- node[["event_duration"]]
  }

  if (is.null(in_node)) {
    dur <- formals(node_time_to_event)[["event_duration"]]
  } else {
    dur <- in_node
  }
  return(dur)
}

## last observation carried forward
na_locf <- function(x) {
  v <- !is.na(x)
  return(c(NA, x[v])[cumsum(v) + 1])
}
