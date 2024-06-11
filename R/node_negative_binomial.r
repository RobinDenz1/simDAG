
## generate data from a negative binomial regression
#' @export
node_negative_binomial <- function(data, parents, formula=NULL, betas,
                                   intercept, theta) {

  if (!data.table::is.data.table(data)) {
    data.table::setDT(data)
  }

  if (!is.null(formula)) {
    data <- stats::model.matrix(object=formula, data=data)
    data <- as.data.frame(data[, -1])
  } else {
    data <- as.data.frame(data[, parents, with=FALSE])
  }

  eta <- intercept +
    rowSums(mapply("*", data, betas))

  out <- stats::rnbinom(n=length(eta), mu=exp(eta), size=theta)

  return(out)
}
