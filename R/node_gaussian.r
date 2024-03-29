
## a node modeled using linear regression
#' @export
node_gaussian <- function(data, parents, formula=NULL, betas,
                          intercept, error) {

  if (!is.null(formula)) {
    data <- stats::model.frame(formula=formula, data=data)
  } else {
    data <- as.data.frame(data[, parents, with=FALSE])
  }

  out <- intercept +
    rowSums(mapply("*", data, betas)) +
    stats::rnorm(n=nrow(data), mean=0, sd=error)
  return(out)
}
