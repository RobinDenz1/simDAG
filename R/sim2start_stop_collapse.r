
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
  max_t <- max(data$stop)

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
