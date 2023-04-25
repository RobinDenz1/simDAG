
## transforms the output of the sim_discrete_time function into a
## single data.table in the long format
#' @export
sim2long <- function(sim, warn=TRUE) {

  # extract node_time_to_event objects
  node_types <- lapply(sim$tx_nodes, FUN=function(x){x$type})
  tte_nodes <- sim$tx_nodes[node_types=="time_to_event"]
  tte_names <- unlist(lapply(tte_nodes, FUN=function(x){x$name}))

  # raise warning due to missing information if needed
  # NOTE: this will be needed once code is implemented for other save_states
  if (length(tte_names) < length(sim$tx_nodes) & sim$save_states!="all" &
      warn) {
    warn_cols <- unlist(lapply(sim$tx_nodes[node_types!="time_to_event"],
                               FUN=function(x){x$name}))
    warning("Resulting data may be inaccurate for the following columns: '",
            paste0(warn_cols, collapse="', '"), "'\nbecause save_states!='all'",
            " in sim_discrete_time function call. See details.")
  }

  if (sim$save_states=="all") {
    data <- sim2long.all(sim=sim, tte_names=tte_names)
  } else if (sim$save_states=="last"){
    data <- sim2long.last(sim=sim, include_tx_nodes=TRUE)
  } else if (sim$save_states=="at_t") {
    stop("This function currently does not work if save_states='at_t'",
         " was used in the original sim_discrete_time() call.")
  }

  return(data)
}

## transform to long-format when save_states="all" was used
#' @importFrom data.table :=
#' @importFrom data.table setcolorder
sim2long.all <- function(sim, tte_names) {

  .id <- .simulation_time <- NULL

  # simply bind together all previous states into one data.table
  data <- data.table::rbindlist(sim$past_states)
  data[, .simulation_time := rep(seq_len(sim$max_t), each=nrow(sim$data))]
  setkey(data, .id, .simulation_time)

  # remove leftover columns
  tte_names_full <- paste0(tte_names, "_time")
  data <- data[, !tte_names_full, with=FALSE]

  # clean others
  colnames(data)[colnames(data) %in% paste0(tte_names, "_event")] <- tte_names

  # reorder columns
  first_cols <- c(".id", ".simulation_time")
  setcolorder(data, c(first_cols,
                      colnames(data)[!colnames(data) %in% first_cols]))

  return(data)
}

## transform to long-format if save_states="last" was used
#' @importFrom data.table :=
#' @importFrom data.table .I
#' @importFrom data.table setcolorder
sim2long.last <- function(sim, include_tx_nodes) {

  .simulation_time <- n_rep <- start <- NULL

  data <- sim2start_stop.last(sim, include_tx_nodes=include_tx_nodes)

  data[, n_rep := stop - start + 1]
  data[start==0, n_rep := n_rep - 1]
  data[, start := NULL]
  data[, stop := NULL]

  data <- data[rep(data[, .I], data$n_rep)]
  data[, n_rep := NULL]

  data[, .simulation_time := rep(seq_len(sim$max_t), nrow(sim$data))]

  # reorder columns
  first_cols <- c(".id", ".simulation_time")
  setcolorder(data, c(first_cols,
                      colnames(data)[!colnames(data) %in% first_cols]))

  return(data)
}
