
## transforms a data.table in the long format to a data.table in
## the start / stop format
#' @importFrom data.table fifelse
#' @importFrom data.table data.table
#' @importFrom data.table setkey
#' @importFrom data.table setkeyv
#' @importFrom data.table shift
#' @importFrom data.table setDT
#' @importFrom data.table setcolorder
#' @importFrom data.table is.data.table
#' @importFrom data.table :=
#' @importFrom data.table .SD
#' @importFrom data.table .N
#' @export
long2start_stop <- function(data, id, time, varying, overlap=FALSE,
                            check_inputs=TRUE) {

  if (!is.data.frame(data)) {
    stop("'data' should be a data.table or an object that can be transformed",
         " to a data.table (data.frame, tibble, ...).")
  }

  # transform to data.table if needed
  if (!is.data.table(data)) {
    setDT(data)
  }

  if (check_inputs) {
    check_inputs_long2start_stop(data=data, id=id, time=time,
                                 varying=varying)
  }

  start <- .is_equal_to_next <- NULL
  max_t <- max(data[[time]])

  setkeyv(data, c(id, time))

  # edge case with no time-varying variables
  if (length(varying)==0) {

    data <- data[eval(parse(text=time))==1,]
    data[, (time) := NULL]
    data[, start := 1]
    data[, stop := max_t]

    first_cols <- c(id, "start", "stop")
    setcolorder(data, c(first_cols,
                        colnames(data)[!colnames(data) %in% first_cols]))
    setkey(data, NULL)

    return(data)
  }

  # identify rows that changed
  data[, .is_equal_to_next := check_next_row_equal(.SD), by=eval(id),
       .SDcols=varying]
  data[, .is_equal_to_next := shift(.is_equal_to_next, type="lag"),
       by=eval((id))]

  # remove un-needed rows
  data <- data[eval(parse(text=time))==1 | eval(parse(text=time))==max_t |
                 !.is_equal_to_next]
  data[, .is_equal_to_next := NULL]

  # assign start and stop
  colnames(data)[colnames(data)==time] <- "start"
  data[, stop := shift(start, type="lead", fill=max_t), by=eval((id))]

  # remove more rows
  data <- unique(data, by=c(id, "stop", varying))

  # correct stop
  data[, stop := fifelse((stop < max_t) | (stop==max_t & seq_len(.N)!=.N),
                         stop - 1, stop, na=stop), by=eval(id)]

  if (overlap) {
    data[, stop := stop + 1]
  }

  # reorder columns
  first_cols <- c(id, "start", "stop", varying)
  setcolorder(data, c(first_cols,
                      colnames(data)[!colnames(data) %in% first_cols]))
  setkey(data, NULL)

  return(data)
}
