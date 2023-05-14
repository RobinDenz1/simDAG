
## efficient bernoulli trials
#' @export
rbernoulli <- function(n, p=0.5) {
  stats::runif(n) > (1 - p)
}

## function to take fast random draws from a multinomial distribution,
## possibly with different probabilities for each individual
#' @export
rcategorical <- function(n, probs, labels=NULL, coerce2factor=FALSE) {

  u <- stats::runif(n=n, min=0, max=1)

  # use the sample function if the probabilities are the same for everyone
  if (!is.matrix(probs)) {
    out <- sample(x=seq(0, (length(probs)-1)), size=n, prob=probs, replace=TRUE)
  # otherwise use custom code
  } else {
    cumsum_probs <- cbind(0, t(apply(probs, 1, cumsum)))

    out <- numeric(n)
    for (i in seq(2, ncol(cumsum_probs))) {
      out <- data.table::fifelse((u >= cumsum_probs[,(i-1)]) &
                                   (u < cumsum_probs[,i]), i-2, out)
    }
  }

  if (coerce2factor & is.null(labels)) {
    out <- factor(out)
  } else if (coerce2factor) {
    out <- factor(out, labels=labels)
  }

  return(out)
}

## simply assign a single constant value
#' @export
rconstant <- function(n, constant) {
  return(rep(constant, n))
}
