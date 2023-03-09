
## transforms the output of the sim_discrete_time function into a
## single data.table in the long format
#' @export
sim2long <- function(sim, warn=TRUE) {

  .id <- .simulation_time <- NULL

  # extract node_time_to_event objects
  node_types <- lapply(sim$tx_nodes, FUN=function(x){x$type})
  tte_nodes <- sim$tx_nodes[node_types=="time_to_event"]
  tte_names <- unlist(lapply(tte_nodes, FUN=function(x){x$name}))

  # raise warning due to missing information if needed
  if (length(tte_names) < length(sim$tx_nodes) & sim$save_states!="all" &
      warn) {
    warn_cols <- unlist(lapply(sim$tx_nodes[node_types!="time_to_event"],
                               FUN=function(x){x$name}))
    warning("Resulting data may be inaccurate for the following columns: '",
            paste0(warn_cols, collapse="', '"), "'\nbecause save_states!='all'",
            " in sim_discrete_time function call. See details.")
  }

  if (sim$save_states=="all") {
    # simply bind together all previous states into one data.table
    data <- data.table::rbindlist(sim$past_states)
    data <- data[order(.id, .simulation_time)]

    # remove leftover columns
    tte_names_full <- c(paste0(tte_names, "_time"),
                        paste0(tte_names, "_past_event_times"))
    data <- data[, !tte_names_full, with=FALSE]

  } else if (sim$save_states=="last"){
    # extract data for other nodes
    tte_names_full <- c(paste0(tte_names, "_event"),
                        paste0(tte_names, "_time"),
                        paste0(tte_names, "_past_event_times"),
                        ".simulation_time")

    data_fixed <- sim$data[, !tte_names_full, with=FALSE]
    data_fixed$.id <- seq(1, nrow(data_fixed))

    # construct long version of each time-to-event node one by one
    data <- vector(mode="list", length=length(tte_nodes))
    for (i in seq_len(length(tte_nodes))) {
      data[[i]] <- sim2long.tte_node(sim=sim, tte_node=tte_nodes[[i]])
    }
    data <- do.call(what="cbind", args=data)

    # merge with time-fixed data
    data <- merge(data, data_fixed, by=".id", all.x=TRUE)

  } else if (sim$save_states=="at_t") {
    # TODO: ??? not sure yet
  }

  return(data)
}

## transforms the output of the sim_discrete_time function of a single
## time-to-event node into the long format, if only the last state
## was saved
sim2long.tte_node <- function(sim, tte_node) {

  .id <- .simulation_time <- NULL

  # specific names
  name_event <- paste0(tte_node$name, "_event")
  name_time <- paste0(tte_node$name, "_time")
  name_past_event_times <- paste0(tte_node$name, "_past_event_times")
  all_names <- c(name_event, name_time, name_past_event_times, ".id")

  # get relevant data at last state
  data <- sim$data[, all_names, with=FALSE]

  # extract past events, if applicable
  if (!all(is.na(data[[name_past_event_times]]))) {
    past_events <- strsplit(data[[name_past_event_times]], split=" ")
  }

  # repeat rows max_t times
  data <- data[rep(seq(nrow(data)), sim$max_t),]

  # assign simulation time
  data$.simulation_time <- rep(seq(1, sim$max_t), each=max(data$.id))

  # sort by id and time
  data <- data[order(.id, .simulation_time)]

  # extract event duration from node info
  if (is.null(tte_node$event_duration)) {
    event_duration <- formals(node_time_to_event)$event_duration
  } else {
    event_duration <- tte_node$event_duration
  }

  if (!all(is.na(data[[name_past_event_times]]))) {
    # NOTE: creates a new vector for name_time including the previous
    #       events. kinda messy tho
    event_times <- vector(mode="list", length=length(past_events))

    # vector should be NA, except when an event happens + event_duration
    # days afterwards it should be the event time
    # NOTE: this is coded in a pretty inefficient fashion and should probably
    #       be re-factored completely. Maybe changing the way previous
    #       events are stored will make this easier
    for (i in seq_len(length(past_events))) {
      past_events_i <- as.numeric(past_events[[i]])
      out_vec <- rep(NA_integer_, sim$max_t)

      if (anyNA(past_events_i)) {
        event_times[[i]] <- out_vec
      } else {
        # get an indicator at which places to put the event time
        ind <- vapply(past_events_i,
                      FUN=function(x, event_duration){
                        list(seq(x, (x + event_duration)))},
                      event_duration=event_duration,
                      FUN.VALUE=vector(mode="list", length=1))
        ind <- unlist(ind)

        # since the event time repeats, we need another vector
        # containing the respective event time
        # needs to be truncated at max_t
        past_events_i <- rep(past_events_i, each=(event_duration+1))
        past_events_i <- past_events_i[ind <= sim$max_t]

        # clean up index as well
        ind <- ind[ind <= sim$max_t]

        # finally assign it
        out_vec[ind] <- past_events_i
        event_times[[i]] <- out_vec
      }
    }

    data[[name_time]] <- unlist(event_times)
  }

  # create correct indicator of whether event was occuring at each point
  # in time
  data[[name_event]] <- (data$.simulation_time >= data[[name_time]]) &
    (data$.simulation_time <= (data[[name_time]] + event_duration))
  data[[name_event]][is.na(data[[name_event]])] <- FALSE

  # remove names
  data[[name_time]] <- NULL
  data[[name_past_event_times]] <- NULL

  return(data)
}
