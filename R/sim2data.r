
## wrapper function for all data transformations involving simDT objects
#' @export
sim2data <- function(sim, to, use_saved_states=sim$save_states=="all",
                     overlap=FALSE, as_data_frame=FALSE,
                     check_inputs=TRUE, ...) {

  if (check_inputs) {
    check_inputs_sim2data(sim=sim, use_saved_states=use_saved_states, to=to)
  }

  if (to=="start_stop") {
    data <- sim2start_stop(sim=sim, use_saved_states=use_saved_states,
                           overlap=overlap)
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
                                overlap=FALSE,
                                use_saved_states=x$save_states=="all",
                                check_inputs=TRUE, ...) {
  out <- sim2data(sim=x, to=to, use_saved_states=use_saved_states,
                  as_data_frame=FALSE, check_inputs=check_inputs,
                  overlap=overlap)
  return(out)
}

## same as sim2data() but extending the as.data.frame() generic instead
#' @export
as.data.frame.simDT <- function(x, row.names=NULL, optional=FALSE,
                                to, overlap=FALSE,
                                use_saved_states=x$save_states=="all",
                                check_inputs=TRUE, ...) {
  out <- sim2data(sim=x, to=to, use_saved_states=use_saved_states,
                  as_data_frame=TRUE, check_inputs=check_inputs,
                  row.names=row.names, optional=optional, overlap=overlap,
                  ...)
  return(out)
}
