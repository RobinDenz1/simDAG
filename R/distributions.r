
## efficient bernoulli trials
#' @export
rbernoulli <- function(n, p=0.5) {
  stats::runif(n) > (1 - p)
}

## generate draws from discrete distribution
#' @export
## function to take fast random draws from a multinomial distribution,
## possibly with different probabilities for each individual
rcategorical <- function(n, probs, labels=NULL) {

  u <- runif(n=n, min=0, max=1)

  # use the sample function if the probabilities are the same for everyone
  if (!is.matrix(probs)) {
    if (is.null(labels)) {
      labels <- seq(1, length(probs))
    }
    out <- sample(x=labels, size=n, prob=probs, replace=TRUE)
    # otherwise use custom code
  } else {
    cumsum_probs <- cbind(0, t(apply(probs, 1, cumsum)))

    out <- numeric(n)
    for (i in seq(2, ncol(cumsum_probs))) {
      out <- data.table::fifelse((u >= cumsum_probs[,(i-1)]) &
                                   (u < cumsum_probs[,i]), i-1, out)
    }

    if (!is.null(labels)) {
      out <- factor(out, labels=labels)
    }
  }

  return(out)
}

## simply assign a single constant value
#' @export
constant <- function(constant) {
  return(constant)
}
