
## wrapper function for all data transformations involving simDT objects
#' @export
sim2data <- function(sim, to, use_saved_states=sim$save_states=="all",
                     overlap=FALSE, target_event=NULL,
                     keep_only_first=FALSE, remove_not_at_risk=FALSE,
                     remove_vars=NULL, as_data_frame=FALSE,
                     check_inputs=TRUE, ...) {

  if (check_inputs) {
    check_inputs_sim2data(sim=sim, use_saved_states=use_saved_states, to=to,
                          overlap=overlap, target_event=target_event,
                          keep_only_first=keep_only_first,
                          remove_not_at_risk=remove_not_at_risk,
                          remove_vars=remove_vars)
  }

  if (!is.null(remove_vars)) {
    sim <- remove_vars_simDT(sim=sim, vars=remove_vars)
  }

  if (to=="start_stop") {
    data <- sim2start_stop(sim=sim, use_saved_states=use_saved_states,
                           overlap=overlap, target_event=target_event,
                           keep_only_first=keep_only_first,
                           remove_not_at_risk=remove_not_at_risk)
  } else if (to=="long") {
    data <- sim2long(sim=sim, use_saved_states=use_saved_states)
  } else if (to=="wide") {
    data <- sim2wide(sim=sim, use_saved_states=use_saved_states)
  }

  if (as_data_frame) {
    data <- as.data.frame(data, ...)
  }

  return(data)
}

## same as sim2data() but extending the as.data.table() generic instead
#' @importFrom data.table as.data.table
#' @export
as.data.table.simDT <- function(x, keep.rownames=FALSE, to,
                                overlap=FALSE, target_event=NULL,
                                keep_only_first=FALSE,
                                remove_not_at_risk=FALSE,
                                remove_vars=NULL,
                                use_saved_states=x$save_states=="all",
                                check_inputs=TRUE, ...) {
  out <- sim2data(sim=x, to=to, use_saved_states=use_saved_states,
                  as_data_frame=FALSE, check_inputs=check_inputs,
                  overlap=overlap, target_event=target_event,
                  keep_only_first=keep_only_first,
                  remove_not_at_risk=remove_not_at_risk,
                  remove_vars=remove_vars)
  return(out)
}

## same as sim2data() but extending the as.data.frame() generic instead
#' @export
as.data.frame.simDT <- function(x, row.names=NULL, optional=FALSE,
                                to, overlap=FALSE, target_event=NULL,
                                keep_only_first=FALSE,
                                remove_not_at_risk=FALSE,
                                remove_vars=NULL,
                                use_saved_states=x$save_states=="all",
                                check_inputs=TRUE, ...) {
  out <- sim2data(sim=x, to=to, use_saved_states=use_saved_states,
                  as_data_frame=TRUE, check_inputs=check_inputs,
                  row.names=row.names, optional=optional, overlap=overlap,
                  target_event=target_event, keep_only_first=keep_only_first,
                  remove_not_at_risk=remove_not_at_risk,
                  remove_vars=remove_vars, ...)
  return(out)
}

## removes some variables from the simDT object before transforming it
## into a usable dataset
remove_vars_simDT <- function(sim, vars) {

  # names and types of time-varying variables
  varying <- unlist(lapply(sim$tx_nodes, FUN=function(x){x$name}))

  if (length(varying) > 0) {
    types <- unlist(lapply(sim$tx_nodes, FUN=function(x){x$type_str}))
    tte_names <- varying[types=="time_to_event"]
    ce_names <- varying[types=="competing_events"]
    tte_names_full <- get_tte_names(tx_node_names=varying[varying %in% vars],
                                    tx_node_types=types[varying %in% vars],
                                    tx_nodes=sim$tx_nodes[varying %in% vars])
    ce_names_full <- get_ce_names(tx_node_names=varying[varying %in% vars],
                                  tx_node_types=types[varying %in% vars])

    # remove those from internal storage
    sim$tx_nodes[varying %in% vars] <- NULL

    if (length(tte_names) > 0) {
      sim$tte_past_events[tte_names %in% vars] <- NULL
    }

    if (length(ce_names) > 0) {
      sim$ce_past_events[ce_names %in% vars] <- NULL
      sim$ce_past_causes[ce_names %in% vars] <- NULL
    }
  }

  # remove 'vars' from last state of the simulation
  rm_vars <- c(vars, tte_names_full, ce_names_full)
  rm_vars <- rm_vars[rm_vars %in% colnames(sim$data)]
  sim$data <- sim$data[, !rm_vars, with=FALSE]

  # if required, also remove them from all saved simulation states
  if (sim$save_states!="last") {
    for (i in seq_len(length(sim$past_states))) {
      sim$past_states[[i]] <- sim$past_states[[i]][, !rm_vars, with=FALSE]
    }
  }

  return(sim)
}
