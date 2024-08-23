
## transform data simulated using the sim_discrete_time function to the
## wide format
sim2wide <- function(sim, use_saved_states=sim$save_states=="all") {

  d_long <- sim2long(sim=sim, use_saved_states=use_saved_states)

  varying_cols <- unlist(lapply(sim$tx_nodes, FUN=function(x){x$name}))

  fixed_cols <- colnames(d_long)[!colnames(d_long) %in% varying_cols]
  fixed_cols <- fixed_cols[fixed_cols!=".time"]

  # failsafe
  varying_cols <- unique(varying_cols)
  fixed_cols <- unique(fixed_cols)

  form <- paste0(paste(fixed_cols, collapse=" + "), " ~ .time")

  data <- data.table::dcast(data=d_long,
                            formula=stats::as.formula(form),
                            value.var=varying_cols)

  # rename columns if there is only one time-varying variable
  if (length(varying_cols)==1) {
    n_t <- ncol(data) - length(fixed_cols)
    cnames <- c(fixed_cols, paste0(varying_cols, 1:n_t, sep="_"))
    colnames(data) <- cnames
  }

  return(data)
}
