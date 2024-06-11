
## a node modeled using linear regression
#' @export
node_gaussian <- function(data, parents, formula=NULL, betas,
                          intercept, error) {

  if (!data.table::is.data.table(data)) {
    data.table::setDT(data)
  }

  if (!is.null(formula)) {
    data <- stats::model.matrix(object=formula, data=data)
    data <- as.data.frame(data[, -1])
  } else {
    data <- as.data.frame(data[, parents, with=FALSE])
  }

  out <- intercept +
    rowSums(mapply("*", data, betas)) +
    stats::rnorm(n=nrow(data), mean=0, sd=error)
  return(out)
}
