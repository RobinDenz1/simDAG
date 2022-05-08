
## efficient bernoulli trials
#' @export
rbernoulli <- function(n, p=0.5) {
  as.numeric(stats::runif(n) > (1 - p))
}

## generate draws from discrete distribution
#' @export
rcategorical <- function(n, labels, probs) {
  sample(x=labels, size=n, prob=prob, replace=TRUE)
}
