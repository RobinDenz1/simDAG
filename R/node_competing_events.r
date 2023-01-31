
## A node to generate arbitrary time-to-event data in
## discrete-time simulations
# TODO:
#   - also needs "past_event_kind" column
#   - allow vector input in event_duration, immunity_duration
#   - get the probability setting right

event_prob <- matrix(c(0.1, 0.2, 0.5, 0.7, 0.4, 0.1), nrow=2)

name <- "thing"
sim_time <- 1
prob_fun_args <- list()
event_duration <- 0
immunity_duration <- 0
save_past_events <- TRUE

rcategorical(n=2, probs=event_prob)

#' @export
node_competing_events <- function(data, parents, sim_time, name, prob_fun,
                                  prob_fun_args=list(), event_duration=0,
                                  immunity_duration=event_duration + 0,
                                  save_past_events=TRUE, labels=NULL) {

  # get list of arguments
  prob_fun_args$data <- data

  if ("sim_time" %in% names(formals(prob_fun))) {
    prob_fun_args$sim_time <- sim_time
  }

  # get event probabilities
  event_prob <- do.call(prob_fun, args=prob_fun_args)

  # specific names
  name_event <- paste0(name, "_event")
  name_time <- paste0(name, "_time")
  name_past_event_times <- paste0(name, "_past_event_times")

  days_since_event <- sim_time - data[[name_time]]

  # set event_prob to 1 if event is ongoing and to 0 if it is no longer
  # ongoing but the person is still immune, keep estimated probability
  # otherwise
  event_prob <- fifelse(is.na(data[[name_time]]), event_prob,
                        fifelse(data[[name_event]] &
                                days_since_event <= event_duration, 1,
                        fifelse(data[[name_event]] &
                                days_since_event <= immunity_duration, 0,
                                event_prob)))

  # steps to solve this for multiple events
  # 1. figure out who has probability 1 and in what column
  # 2. construct a matrix with one column per person that needs their probs changed
  # 3. use this to set that probability to one
  # 4. create the opposite of that matrix for those persons, set that prob to 0
  ids <- seq_len(nrow(data))

  # rows where the probability should be one
  ids_1 <- ids[!is.na(data[[name_time]]) && (days_since_event <= event_duration)]
  m_1 <- matrix(c(ids_1, data[[name_event]][ids_1]), nrow=length(ids_1),
                byrow=TRUE)

  # rows where the probability should be zero
  ids_0 <- ids[!is.na(data[[name_time]]) &&
               (days_since_event <= immunity_duration) &&
               !days_since_event <= event_duration]
  m_0 <- matrix(c(ids_0, data[[name_event]][ids_0]), nrow=length(ids_0),
                byrow=TRUE)




  # draw new events based on this probability
  event <- rcategorical(n=nrow(data), p=event_prob)

  # update event time
  event_time <- fifelse(is.na(data[[name_time]]) & event, sim_time,
                        fifelse(is.na(data[[name_time]]) &
                                !event, NA_integer_,
                        fifelse(!is.na(data[[name_time]]) &
                                days_since_event <= immunity_duration,
                                data[[name_time]], NA_integer_)))

  # update past event times
  # NOTE: I don't like this but I have not yet figured out a better way to
  #       do this. Hash tables were actually much slower.
  if (save_past_events) {
    past_events <- data[[name_past_event_times]]
    past_events[!is.na(event_time) & event_time==sim_time &
                !is.na(past_events)] <-
      paste(past_events[!is.na(event_time) & event_time==sim_time &
                        !is.na(past_events)], sim_time)
    past_events[!is.na(event_time) & event_time==sim_time &
                is.na(past_events)] <- as.character(sim_time)
  } else {
    past_events <- NA
  }

  # put together
  out <- data.table::data.table(event=event,
                                event_time=event_time,
                                past_events=past_events)
  colnames(out) <- c(name_event, name_time, name_past_event_times)

  return(out)
}
