
## a node modeled using poisson-regression
#' @export
node_poisson <- function(data, parents, formula=NULL, betas, intercept) {

  if (!data.table::is.data.table(data)) {
    data.table::setDT(data)
  }

  if (!is.null(formula)) {
    data <- stats::model.matrix(object=formula, data=data)
    data <- as.data.frame(data[, -1])
  } else {
    data <- as.data.frame(data[, parents, with=FALSE])
  }

  eff <- rowSums(mapply("*", data, betas))
  mu <- exp(intercept + eff)
  out <- vapply(mu, FUN=stats::rpois, FUN.VALUE=numeric(1), n=1)

  return(out)
}
