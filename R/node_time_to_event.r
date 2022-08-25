
## a node format to generate arbitrary time-to-event data
#' @export
node_time_to_event <- function(data, parents, sim_time, name, prob_fun,
                               prob_fun_args=list(), event_duration=0,
                               immunity_duration=event_duration + 0,
                               save_past_events=TRUE) {

  # get list of arguments
  prob_fun_args$data <- data[, parents, drop=FALSE]

  if ("sim_time" %in% names(formals(prob_fun))) {
    prob_fun_args$sim_time <- sim_time
  }

  # get event probabilities
  event_prob <- do.call(prob_fun, args=prob_fun_args)

  # specific names
  name_event <- paste0(name, "_event")
  name_time <- paste0(name, "_time")
  name_past_event_times <- paste0(name, "_past_event_times")

  days_since_event <- sim_time - data[, name_time]

  # set event_prob to 1 if event is ongoing and to 0 if it is no longer
  # ongoing but the person is still immune, keep estimated probability
  # otherwise
  event_prob <- ifelse(is.na(data[, name_time]), event_prob,
                       ifelse(data[, name_event] &
                              days_since_event <= event_duration,
                              1,
                       ifelse(data[, name_event] &
                              days_since_event <= immunity_duration,
                              0, event_prob)))

  # draw new events based on this probability
  event <- rbernoulli(n=nrow(data), p=event_prob)

  # update event time
  event_time <- ifelse(is.na(data[, name_time]) & event, sim_time,
                       ifelse(is.na(data[, name_time]) & !event, NA,
                       ifelse(!is.na(data[, name_time]) &
                              days_since_event <= immunity_duration,
                              data[, name_time], NA)))

  # update past event times
  if (save_past_events) {
    past_events <- data[, name_past_event_times]
    past_events[!is.na(event_time) &
                  event_time==sim_time &
                  !is.na(past_events)] <-
      paste(past_events[!is.na(event_time) &
                        event_time==sim_time &
                        !is.na(past_events)], sim_time)
    past_events[!is.na(event_time) &
                  event_time==sim_time &
                  is.na(past_events)] <- as.character(sim_time)
  } else {
    past_events <- NA
  }

  # put together
  out <- data.frame(event=event, event_time=event_time, past_events=past_events)
  colnames(out) <- c(name_event, name_time, name_past_event_times)

  return(out)
}
