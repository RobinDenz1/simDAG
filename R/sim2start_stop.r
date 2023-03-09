
## transform output from the sim_discrete_time function into the start-stop
## format
#' @export
sim2start_stop <- function(sim, tte_name, warn=TRUE) {

  # transform to long format
  data <- sim2long(sim=sim, warn=warn)

  # change name
  tte_name_orig <- tte_name
  tte_name <- paste0(tte_name, "_event")

  # get names of time-varying variables
  tx_names <- unlist(lapply(sim$tx_nodes, FUN=function(x){x$name}))
  tx_type <- unlist(lapply(sim$tx_nodes, FUN=function(x){x$type}))
  non_tte_names <- tx_names[tx_type!="time_to_event"]

  # get t0_data
  data_fixed <- sim$data[, !c(paste0(tte_name_orig, "_event"),
                              paste0(tte_name_orig, "_time"),
                              paste0(tte_name_orig, "_past_event_times"),
                              non_tte_names, ".simulation_time"),
                         with=FALSE]

  # transform to start-stop
  data <- long_to_periods(data, .id=".id", .start=".simulation_time",
                          .by=c(tte_name, non_tte_names))
  data <- data.table::as.data.table(data)

  # merge t0_data to it
  data <- merge(data, data_fixed, by=".id", all.x=TRUE)

  return(data)
}

## function taken (and slightly changed) from
## https://github.com/larmarange/JLutils/
long_to_periods <- function(data, .id, .start, .by=NULL) {

  start <- .grp <- .prev_grp <- .prev_stop <- .next_prev_stop <- .last_stop <-
    NULL

  data$start <- data[[.start]]

  data <- data %>%
    dplyr::arrange(.data[[.id]], .data[[.start]]) %>%
    dplyr::group_by(!!!dplyr::syms(c(.id, .by)))
  data$.grp <- data %>% dplyr::group_indices()

  data <- data %>%
    dplyr::group_by(!!!dplyr::syms(.id)) %>%
    dplyr::mutate(stop=dplyr::lead(start)) %>%
    dplyr::filter(!is.na(stop)) %>%
    dplyr::group_by(!!!dplyr::syms(.id)) %>%
    dplyr::mutate(.prev_grp=dplyr::lag(.grp),
                  .prev_stop=dplyr::lag(stop))

  periods <- data %>%
    dplyr::filter(is.na(.prev_grp) | .grp!=.prev_grp | start!=.prev_stop) %>%
    dplyr::group_by(!!!dplyr::syms(.id)) %>%
    dplyr::mutate(.next_prev_stop=dplyr::lead(.prev_stop))

  # trick: using the next value of .prev_stop allows to identify
  # the new value of stop in periods if no next value, stop remains unchanged

  temp_dat <- data %>%
    dplyr::group_by(!!!dplyr::syms(.id)) %>%
    dplyr::summarise(.last_stop=max(stop, na.rm=TRUE))

  periods <- merge(periods, temp_dat, by=.id, all.x=TRUE) %>%
    dplyr::mutate(stop=ifelse(!is.na(.next_prev_stop),
                              .next_prev_stop, .last_stop))
  class(periods$stop) <- class(periods$.next_prev_stop) # bug fix

  periods <- periods[, c(.id, "start", "stop", .by)]

  return(periods)
}
