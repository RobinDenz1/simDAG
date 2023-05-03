
## wrapper function for all data transformations involving simDT objects
#' @export
sim2data <- function(sim, to, use_saved_states=sim$save_states=="all",
                     check_inputs=TRUE) {

  if (check_inputs) {
    check_inputs_sim2data(sim=sim, use_saved_states=use_saved_states, to=to)
  }

  if (to=="start_stop") {
    data <- sim2start_stop(sim=sim, use_saved_states=use_saved_states)
  } else if (to=="long") {
    data <- sim2long(sim=sim, use_saved_states=use_saved_states)
  } else if (to=="wide") {
    data <- sim2wide(sim=sim, use_saved_states=use_saved_states)
  }

  return(data)
}
