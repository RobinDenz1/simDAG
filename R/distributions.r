
## efficient bernoulli trials
#' @export
rbernoulli <- function(n, p=0.5) {
  stats::runif(n) > (1 - p)
}

## generate draws from discrete distribution
#' @export
rcategorical <- function(n, labels, probs) {
  sample(x=labels, size=n, prob=probs, replace=TRUE)
}

## simply assign a single constant value
#' @export
constant <- function(constant) {
  return(constant)
}
