
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
    stop("This function currently does not work if save_states='last'",
         " was used in the original sim_discrete_time() call.")
  } else if (sim$save_states=="at_t") {
    stop("This function currently does not work if save_states='at_t'",
         " was used in the original sim_discrete_time() call.")
  }

  return(data)
}

## used when save_states="all" was used in sim_discrete_time
sim2long.all <- function(sim, tte_names) {

  # simply bind together all previous states into one data.table
  data <- data.table::rbindlist(sim$past_states)
  setkey(data, .id, .simulation_time)

  # remove leftover columns
  tte_names_full <- paste0(tte_names, "_time")
  data <- data[, !tte_names_full, with=FALSE]

  # clean others
  colnames(data)[colnames(data) %in% paste0(tte_names, "_event")] <- tte_names

  return(data)
}
