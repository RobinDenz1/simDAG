
## transform data simulated using the sim_discrete_time function to the
## wide format
sim2wide <- function(sim, use_saved_states=sim$save_states=="all") {

  d_long <- sim2long(sim=sim, use_saved_states=use_saved_states)

  node_types <- lapply(sim$tx_nodes, FUN=function(x){x$type})
  tte_nodes <- sim$tx_nodes[node_types=="time_to_event"]
  tte_names <- unlist(lapply(tte_nodes, FUN=function(x){x$name}))
  tx_names <- unlist(lapply(sim$tx_nodes, FUN=function(x){x$name}))

  fixed_cols <- colnames(d_long)[!colnames(d_long) %in% tx_names]
  fixed_cols <- fixed_cols[fixed_cols!=".time"]

  if (sim$save_states=="all") {
    varying_cols <- tx_names
  } else {
    varying_cols <- tte_names
    fixed_cols <- c(fixed_cols, tx_names[!tx_names %in% tte_names])
  }

  # failsafe
  varying_cols <- unique(varying_cols)
  fixed_cols <- unique(fixed_cols)

  form <- paste0(paste(fixed_cols, collapse=" + "), " ~ .time")

  data <- data.table::dcast(data=d_long,
                            formula=stats::as.formula(form),
                            value.var=varying_cols)

  return(data)
}
