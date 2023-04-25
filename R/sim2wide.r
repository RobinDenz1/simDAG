
## transform data simulated using the sim_discrete_time function to the
## wide format
#' @export
sim2wide <- function(sim, warn=TRUE) {

  d_long <- sim2long(sim=sim, warn=warn)

  node_types <- lapply(sim$tx_nodes, FUN=function(x){x$type})
  tte_nodes <- sim$tx_nodes[node_types=="time_to_event"]
  tte_names <- unlist(lapply(tte_nodes, FUN=function(x){x$name}))
  tx_names <- unlist(lapply(sim$tx_nodes, FUN=function(x){x$name}))

  fixed_cols <- colnames(d_long)[!colnames(d_long) %in% tx_names]
  fixed_cols <- fixed_cols[fixed_cols!=".simulation_time"]

  if (sim$save_states=="all") {
    varying_cols <- tx_names
  } else {
    varying_cols <- tte_names
    fixed_cols <- c(fixed_cols, tx_names[!tx_names %in% tte_names])
  }

  # failsafe
  varying_cols <- unique(varying_cols)
  fixed_cols <- unique(fixed_cols)

  form <- paste0(paste(fixed_cols, collapse=" + "), " ~ .simulation_time")

  data <- data.table::dcast(data=d_long,
                            formula=stats::as.formula(form),
                            value.var=varying_cols)

  return(data)
}
