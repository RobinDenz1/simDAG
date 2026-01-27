
## efficient bernoulli trials
#' @export
rbernoulli <- function(n, p=0.5, output="logical", reference=NULL) {
  out <- stats::runif(n) > (1 - p)

  if (output=="numeric") {
    out <- as.numeric(out)
  } else if (output=="character") {
    out <- as.character(out)
  } else if (output=="factor") {
    out <- as.factor(out)
    if (!is.null(reference) && reference %in% out) {
      out <- stats::relevel(out, ref=reference)
    }
  }

  return(out)
}

## function to take fast random draws from a multinomial distribution,
## possibly with different probabilities for each individual
#' @export
rcategorical <- function(n, probs, labels=NULL, output="numeric",
                         reference=NULL) {

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

  # needed for setting labels if not all events occurred
  if (!is.null(labels)) {
    observed <- sort(unique(out)) + 1
  }

  if (output=="factor" & is.null(labels)) {
    out <- factor(out)
  } else if (output=="factor") {
    out <- factor(out, labels=labels[observed])
  } else if (!is.null(labels)) {
    out <- as.character(factor(out, labels=labels[observed]))
  } else if (output=="character") {
    out <- as.character(out)
  }

  if (output=="factor" && !is.null(reference) && reference %in% out) {
    out <- stats::relevel(out, ref=reference)
  }

  return(out)
}

## simply assign a single constant value
#' @export
rconstant <- function(n, constant) {
  return(rep(constant, n))
}

## simply sample from x to generate a node
#' @export
rsample <- function(n, x, replace=FALSE, prob=NULL) {

  if (is.data.frame(x)) {
    ind <- sample.int(n=nrow(x), size=n, replace=replace, prob=prob)
    out <- x[ind, ]
  } else {
    out <- sample(x=x, size=n, replace=replace, prob=prob)
  }

  return(out)
}

## generate random values from a left truncated exponential distribution
#' @export
rtexp <- function (n, rate, l=NULL)  {

  stopifnot("All values in 'rate' should be > 0." = all(rate > 0))

  if (!is.null(l)) {
    l - log(1 - stats::runif(n))/rate
  } else {
    stats::rexp(n=n, rate=rate)
  }
}
