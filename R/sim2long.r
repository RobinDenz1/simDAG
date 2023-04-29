
## transforms the output of the sim_discrete_time function into a
## single data.table in the long format
#' @export
sim2long <- function(sim, use_saved_states=sim$save_states=="all",
                     check_inputs=TRUE) {

  if (check_inputs) {
    check_inputs_sim2data(sim=sim, use_saved_states=use_saved_states,
                          include_tx_nodes=FALSE)
  }

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

  # reorder columns
  first_cols <- c(".id", ".time")
  setcolorder(data, c(first_cols,
                      colnames(data)[!colnames(data) %in% first_cols]))

  return(data)
}
