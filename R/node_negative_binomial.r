
## generate data from a negative binomial regression
#' @export
node_negative_binomial <- function(data, parents, formula=NULL, betas, theta) {

  if (!is.null(formula)) {
    data <- stats::model.frame(formula=formula, data=data)
  } else {
    data <- as.data.frame(data[, parents, with=FALSE])
  }

  eta <- as.matrix(data) %*% betas

  out <- stats::rnbinom(n=length(eta), mu=exp(eta), size=theta)

  return(out)
}

