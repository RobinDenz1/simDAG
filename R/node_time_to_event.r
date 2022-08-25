
## a node format to generate arbitrary time-to-event data
# TODO: figure out how to efficiently finish this function
#' @export
node_time_to_event <- function(data, parents, sim_time, prob_fun,
                               prob_fun_args=list(),
                               recurrent=TRUE, event_duration=1,
                               immunity_duration=0) {

  # get list of arguments
  prob_fun_args$data <- data[, parents]
  prob_fun_args$sim_time <- sim_time

  # get event probabilities
  event_prob <- do.call(prob_fun, args=prob_fun_args)

  # based on past data, transform it according to arguments
  name_event <- paste0(name, "_event")
  name_time <- paste0(name, "_time")
  name_past_times <- paste0(name, "_past_times")

  # TODO: There is still a lot missing in this function.
  #   - needs access to events, and event times, both of
  #     this event itself and other time-to-event nodes
  #   - the past event times could be implemented using a hash table
  #   - needs an automatic way to implement immunity and event durations,
  #     based on those previous values
  #   - finally, one bernoulli call

}
