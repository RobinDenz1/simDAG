
## transforms a data.table in the long format to a data.table in
## the start / stop format
#' @importFrom data.table fifelse
#' @importFrom data.table data.table
#' @importFrom data.table setkey
#' @importFrom data.table setkeyv
#' @importFrom data.table shift
#' @importFrom data.table setDT
#' @importFrom data.table setnames
#' @importFrom data.table setcolorder
#' @importFrom data.table is.data.table
#' @importFrom data.table :=
#' @importFrom data.table .SD
#' @importFrom data.table .N
#' @export
long2start_stop <- function(data, id, time, varying, overlap=FALSE,
                            check_inputs=TRUE) {

  ..id.. <- ..time.. <- NULL

  if (!is.data.frame(data)) {
    stop("'data' should be a data.table or an object that can be transformed",
         " to a data.table (data.frame, tibble, ...).", call.=FALSE)
  }

  requireNamespace("data.table", quietly=TRUE)

  # transform to data.table if needed
  if (!is.data.table(data)) {
    data <- as.data.table(data)
  } else {
    data <- copy(data)
  }

  if (check_inputs) {
    check_inputs_long2start_stop(data=data, id=id, time=time,
                                 varying=varying)
  }

  setnames(data, old=c(id, time), new=c("..id..", "..time.."))

  start <- .is_equal_to_next <- NULL
  max_t <- max(data$..time..)

  setkeyv(data, c("..id..", "..time.."))

  # edge case with no time-varying variables
  if (length(varying)==0) {

    data <- data[..time..==1,]
    data[, ..time.. := NULL]
    data[, start := 1]
    data[, stop := max_t]

    first_cols <- c("..id..", "start", "stop")
    setcolorder(data, c(first_cols,
                        colnames(data)[!colnames(data) %in% first_cols]))
    setkey(data, NULL)
    setnames(data, old="..id..", new=id)

    return(data)
  }

  # identify rows that changed
  data[, .is_equal_to_next := check_next_row_equal(.SD), by="..id..",
       .SDcols=varying]
  data[, .is_equal_to_next := shift(.is_equal_to_next, type="lag"),
       by=..id..]

  # remove un-needed rows
  data <- data[..time..==1 | ..time..==max_t | !.is_equal_to_next]
  data[, .is_equal_to_next := NULL]

  # assign start and stop
  setnames(data, old="..time..", new="start")
  data[, stop := shift(start, type="lead", fill=max_t), by="..id.."]

  # remove more rows
  data <- unique(data, by=c("..id..", "stop", varying))

  # correct stop
  data[, stop := fifelse((stop < max_t) | (stop==max_t & seq_len(.N)!=.N),
                         stop - 1, stop, na=stop), by="..id.."]

  if (overlap) {
    data[, stop := stop + 1]
  }

  # reorder columns
  first_cols <- c("..id..", "start", "stop", varying)
  setcolorder(data, c(first_cols,
                      colnames(data)[!colnames(data) %in% first_cols]))
  setkey(data, NULL)
  setnames(data, old="..id..", new=id)

  return(data)
}
