
## transforms a data.table in the long format to a data.table in
## the start / stop format
#' @importFrom data.table fifelse
#' @importFrom data.table data.table
#' @importFrom data.table setkeyv
#' @importFrom data.table shift
#' @importFrom data.table setDT
#' @importFrom data.table setcolorder
#' @export

#data <- subset(d_long, .id==420)
#id <- ".id"
#time <- ".simulation_time"
#varying <- c("A", "B", "C")

long2start_stop <- function(data, id, time, varying) {

  max_t <- max(data[[time]])

  # add time -1 and time 0
  data_0 <- data[data[[time]] <= 2,]
  setkeyv(data_0, cols=c(id, time))
  data_0[, (time) := rep(c(-1, 0), (nrow(data_0)/2))]

  for (i in seq_len(length(varying))) {
    col <- varying[i]
    data_0[, (col) := FALSE]
  }

  data <- setDT(rbind(data, data_0))
  setkeyv(data, cols=c(id, time))

  # log in which rows something changed in the varying variables
  for (i in seq_len(length(varying))) {
    name <- varying[i]
    name_shift <- paste0(varying[i], "_shift")
    name_n_diff <- paste0("n_diff_", name)

    data[, (name_shift) := shift(eval(parse(text=name)),
                                 type="lead", fill=NA), by=eval((id))]
    data[, (name_n_diff) := cumsum(eval(parse(text=name)) !=
                                   eval(parse(text=name_shift))), by=eval((id))]
    data[(time)==0, (name_n_diff) := 0]
    data[, (name) := NULL]
  }

  colnames(data)[colnames(data) %in% paste0(varying, "_shift")] <- varying

  # remove rows without changes
  by_cols <- colnames(data)[!colnames(data) %in% c(varying, time)]
  data <- unique(data, by=by_cols)
  setkeyv(data, cols=c(id, time))

  # remove unneeded columns
  n_diff_names <- paste0("n_diff_", varying)
  data[, c(n_diff_names) := NULL]

  # assign start and stop
  colnames(data)[colnames(data)==time] <- "start"
  data[, stop := shift(start, type="lead", fill=max_t), by=eval((id))]

  # fix columns
  data <- data[start!=stop]
  data[, start := start + 1]
  data$stop <- fifelse(data$stop==max_t, data$stop, data$stop + 1)
  data <- data[start!=stop]

  # reorder columns
  first_cols <- c(id, "start", "stop", varying)
  setcolorder(data, c(first_cols,
                      colnames(data)[!colnames(data) %in% first_cols]))

  return(data)
}
