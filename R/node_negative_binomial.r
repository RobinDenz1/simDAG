
## generate data from a negative binomial regression
#' @export
node_negative_binomial <- function(data, parents, betas, theta) {

  eta <- as.matrix(data[, parents, with=FALSE]) %*% betas

  out <- stats::rnbinom(n=length(eta), mu=exp(eta), size=theta)

  return(out)
}

