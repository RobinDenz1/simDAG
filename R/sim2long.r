
## transforms the output of the sim_discrete_time function into a
## single data.table in the long format
sim2long <- function(sim, use_saved_states=sim$save_states=="all") {

  if (use_saved_states) {
    data <- sim2long.all(sim=sim)
  } else {
    data <- sim2long.last(sim=sim)
  }

  return(data)
}

## transform to long-format when save_states="all" was used
#' @importFrom data.table :=
#' @importFrom data.table setcolorder
sim2long.all <- function(sim) {

  .id <- .time <- NULL
  tte_names <- names(sim$tte_past_events)

  # simply bind together all previous states into one data.table
  data <- data.table::rbindlist(sim$past_states)
  data[, .time := rep(seq_len(sim$max_t), each=nrow(sim$data))]
  setkey(data, .id, .time)

  # remove leftover columns
  tte_names_full <- paste0(tte_names, "_time")
  data <- data[, !tte_names_full, with=FALSE]

  # clean others
  colnames(data)[colnames(data) %in% paste0(tte_names, "_event")] <- tte_names

  # reorder columns
  first_cols <- c(".id", ".time")
  setcolorder(data, c(first_cols,
                      colnames(data)[!colnames(data) %in% first_cols]))

  return(data)
}

## transform to long-format if save_states="last" was used
#' @importFrom data.table :=
#' @importFrom data.table .I
#' @importFrom data.table setcolorder
sim2long.last <- function(sim) {

  .time <- n_rep <- start <- NULL

  data <- sim2start_stop.last(sim)

  data[, n_rep := stop - start + 1]
  data[start==0, n_rep := n_rep - 1]
  data[, start := NULL]
  data[, stop := NULL]

  data <- data[rep(data[, .I], data$n_rep)]
  data[, n_rep := NULL]

  data[, .time := rep(seq_len(sim$max_t), nrow(sim$data))]

  data <- add_optional_cols_long.last(data=data, tx_nodes=sim$tx_nodes)

  # reorder columns
  first_cols <- c(".id", ".time")
  setcolorder(data, c(first_cols,
                      colnames(data)[!colnames(data) %in% first_cols]))
  setkey(data, .id, .time)

  return(data)
}

## nodes of type time_to_event can have additional time-varying columns
## that need to be added manually to the long-form dataset if only the
## last simulation state was saved. This function does that
#' @importFrom data.table :=
#' @importFrom data.table shift
#' @importFrom data.table fifelse
#' @importFrom data.table .N
add_optional_cols_long.last <- function(data, tx_nodes) {

  .id <- NULL

  # identify which nodes have optional columns
  tx_nodes_type <- vapply(tx_nodes, FUN=function(x){x$type},
                          FUN.VALUE=character(1))
  tx_nodes <- tx_nodes[tx_nodes_type=="time_to_event"]

  if (length(tx_nodes) > 0) {
    tx_names <- vapply(tx_nodes, FUN=function(x){x$name},
                       FUN.VALUE=character(1))

    # check if they do have an optional column
    has_time_since_last <- vapply(tx_nodes,
                                  FUN=node_has_arg,
                                  FUN.VALUE=logical(1),
                                  arg="time_since_last",
                                  arg_is_true=TRUE)
    has_event_count <- vapply(tx_nodes,
                              FUN=node_has_arg,
                              FUN.VALUE=logical(1),
                              arg="event_count",
                              arg_is_true=TRUE)

    # add event counts
    if (any(has_event_count | has_time_since_last)) {

      rel_cols <- tx_names[has_event_count | has_time_since_last]
      for (i in seq_len(length(rel_cols))) {

        orig_name <- rel_cols[i]
        new_name <- paste0(orig_name, "_event_count")
        name_shift <- paste0(new_name, "_shift")

        data[, (name_shift) := shift(eval(parse(text=orig_name)),
                                     type="lag", fill=NA), by=.id]
        data[, (new_name) := fifelse(data$.time==1 & data[[orig_name]], 1,
                                    fifelse(!data[[name_shift]] &
                                            data[[orig_name]], 1, 0, na=0))]
        data[, (new_name) := cumsum(eval(parse(text=new_name))), by=.id]
        data[, (name_shift) := NULL]
      }
    }

    # add time since last event
    if (any(has_time_since_last)) {

      rel_cols <- tx_names[has_time_since_last]
      for (i in seq_len(length(rel_cols))) {

        new_name <- paste0(rel_cols[i], "_time_since_last")
        count_name <- paste0(rel_cols[i], "_event_count")

        data[[new_name]] <- NA_integer_
        data[, (new_name) := seq_len(.N) - 1, by=c(".id", count_name)]
        data[eval(parse(text=count_name))==0, (new_name) := NA_integer_]

        if (!has_event_count[i]) {
          data[, (count_name) := NULL]
        }
      }
    }
  }

  return(data)
}
