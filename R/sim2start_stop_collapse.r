
## given a data.table in the start-stop format as created by the
## sim2start_stop() function, create a new data.table in the start-stop
## format that is centric to an outcome (e.g. recurrent-events format)
## or similar stuff
#' @importFrom data.table fifelse
#' @importFrom data.table merge.data.table
#' @importFrom data.table .SD
#' @importFrom data.table :=
#' @importFrom data.table shift
#' @importFrom data.table copy
#' @importFrom data.table setkey
collapse_for_target_event <- function(data, target_event,
                                      keep_only_first=FALSE) {

  # silence devtools check()
  .is_equal_to_next <- .event_lag <- .change_row <- .change_row_shift <- NULL
  . <- .time <- .id <- start <- stop <- NULL

  data <- copy(data)

  # get indicator whether the current row is equal to the next row,
  # including all columns of interest - minus target_event
  data[, .is_equal_to_next := check_next_row_equal(.SD), by=.id,
       .SDcols=setdiff(names(data), c(".id", "start", "stop", target_event))]
  data[is.na(.is_equal_to_next), .is_equal_to_next := FALSE]

  # get lagged version of target event
  data[, .event_lag := shift(eval(parse(text=target_event)), -1, fill=FALSE)]

  # update stop and target event
  data[, .change_row := .event_lag==TRUE & .is_equal_to_next==TRUE &
        eval(parse(text=target_event))==FALSE]
  data[.change_row==TRUE, stop := stop + 1]
  data[.change_row==TRUE, eval(target_event) := TRUE]

  # remove rows no longer needed
  data[, .change_row_shift := shift(.change_row, type="lag", fill=FALSE),
       by=.id]
  data <- data[.change_row_shift==FALSE]

  # remove temporary columns
  data[, .change_row := NULL]
  data[, .change_row_shift := NULL]
  data[, .event_lag := NULL]
  data[, .is_equal_to_next := NULL]

  # remove everything after the first event if specified
  if (keep_only_first) {
    d_first <- data[, .(.time = fifelse(any(eval(parse(text=target_event))),
                      stop[which(eval(parse(text=target_event)))[1]], Inf)),
                    by=.id]
    data <- merge.data.table(data, d_first, by=".id")
    data <- data[stop <= .time]
    data[, .time := NULL]
    setkey(data, NULL)
  }
  return(data)
}

## check if each row is equal to the next row
check_next_row_equal <- function(x) {
  shift_x <- data.table::shift(x, n=1, type="lead", fill=NA)
  return(rowSums(x==shift_x)==ncol(x))
}

## remove time not at-risk periods from start-stop data
#' @importFrom data.table fifelse
#' @importFrom data.table shift
#' @importFrom data.table :=
remove_not_at_risk <- function(data, duration, target_event, overlap,
                               continuous=FALSE) {

  .last_event <- .id <- .start_in_event <- .stop_in_event <- start <- NULL

  # if overlapping ones are supplied, simply re-transform to non-overlapping
  # start-stop format, perform the transformation and re-add the + 1
  if (overlap & !continuous) {
    data[, stop := stop - 1]
  }

  # get last event time
  data[, .last_event := na_locf(fifelse(eval(parse(text=target_event))==TRUE,
                                             stop, NA)), by=.id]
  data[, .last_event := shift(.last_event, n=1), by=.id]

  if (continuous) {
    .max_value <- Inf
  } else {
    .max_value <- .Machine$integer.max
  }
  data[is.na(.last_event), .last_event := .max_value]

  # check if start is during an event
  if (continuous) {
    data[, .stop_in_event := stop > .last_event &
           stop <= (.last_event + duration)]
  } else {
    data[, .start_in_event := start > .last_event &
           start < (.last_event + duration)]
  }

  # check if stop is during an event
  if (continuous) {
    data[, .start_in_event := start >= .last_event &
           start < (.last_event + duration)]
  } else {
    data[, .stop_in_event := stop > .last_event &
           stop < (.last_event + duration)]
  }

  # remove row if it is entirely during an event
  data <- data[!(.start_in_event==TRUE & .stop_in_event==TRUE)]

  # change start if only partially in there
  data[.start_in_event==TRUE, start := .last_event + duration]

  if (overlap & !continuous) {
    data[, stop := stop + 1]
  }

  data[, .last_event := NULL]
  data[, .start_in_event := NULL]
  data[, .stop_in_event := NULL]

  return(data)
}
