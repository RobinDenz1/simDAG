
## check if each row is equal to the next row
check_next_row_equal <- function(x) {
  shift_x <- data.table::shift(x, n=1, type="lead", fill=NA)
  return(rowSums(x==shift_x)==ncol(x))
}

## given a data.table in the start-stop format as created by the
## sim2start_stop() function, create a new data.table in the start-stop
## format that is centric to an outcome (e.g. recurrent-events format)
## or similar stuff
# TODO:
#   - check if this works when there are two events right after each other
#   - this only works if there are no duplicate rows to begin with,
#     which is sadly not the case with current sim2start_stop.last()
#' @importFrom data.table fifelse
#' @importFrom data.table merge.data.table
#' @importFrom data.table .SD
#' @importFrom data.table .SDcols
#' @importFrom data.table :=
#' @importFrom data.table shift
#' @importFrom data.table copy
collapse_for_target_event <- function(d, target_event, keep_only_first=FALSE) {

  d <- copy(d)
  max_t <- max(d$stop)

  # get indicator whether the current row is equal to the next row,
  # including all columns of interest - minus target_event
  d[, .is_equal_to_next := check_next_row_equal(.SD), by=.id,
    .SDcols=setdiff(names(d), c(".id", "start", "stop", target_event))]
  d[is.na(.is_equal_to_next), .is_equal_to_next := FALSE]

  # get lagged version of target event
  d[, .event_lag := shift(eval(parse(text=target_event)), -1, fill=FALSE)]

  # update stop and target event
  d[, .change_row := .event_lag==TRUE & .is_equal_to_next==TRUE]
  d[.change_row==TRUE, stop := stop + 1]
  d[.change_row==TRUE, eval(target_event) := TRUE]

  # remove rows no longer needed
  d[, .change_row_shift := shift(.change_row, type="lag", fill=FALSE), by=.id]
  d <- d[.change_row_shift==FALSE]

  # remove temporary columns
  d[, .change_row := NULL]
  d[, .change_row_shift := NULL]
  d[, .event_lag := NULL]
  d[, .is_equal_to_next := NULL]

  # remove everything after the first event if specified
  if (keep_only_first) {
    d_first <- d[, .(.time = fifelse(any(eval(parse(text=target_event))),
                 stop[which(eval(parse(text=target_event)))[1]], Inf)), by=.id]
    d <- merge.data.table(d, d_first, by=".id")
    d <- d[stop <= .time]
    d[, .time := NULL]
  }
  return(d)
}
