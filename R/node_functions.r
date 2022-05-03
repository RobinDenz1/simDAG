
## efficient bernoulli trials
#' @export
rbernoulli <- function(n, p=0.5) {
  as.numeric(stats::runif(n) > (1 - p))
}

## simulate survival time according to Bender et al. (2005)
sim_surv_time <- function(row, betas, dist, lambda, gamma) {
  U <- stats::runif(1, min=0, max=1)
  eff <- sum(row * betas)

  if (dist=="weibull") {
    surv_time <- (-(log(U)/(lambda*exp(eff))))^(1/gamma)
  } else if (dist=="exponential") {
    surv_time <- -(log(U)/(lambda*exp(eff)))
  }
  return(surv_time)
}

## a node modeled using linear regression
#' @export
node_linear <- function(data, parents, betas, intercept, error) {
  out <- intercept +
    rowSums(mapply("*", as.data.frame(data[, parents]), betas)) +
    stats::rnorm(n=nrow(data), mean=0, sd=error)
  return(out)
}

## a node modeled using logistic regression
#' @export
node_binomial <- function(data, parents, betas, intercept) {
  prob <- intercept +
    rowSums(mapply("*", as.data.frame(data[, parents]), betas))
  prob <- 1/(1 + exp(-prob))
  out <- rbernoulli(n=nrow(data), p=prob)
  return(out)
}

## a node modeled using multinomial regression
# NOTE: betas must be a matrix with length(parents) columns and
#       n_classes rows
# TODO: This works, but its unclear how to incorporate this into the
#       other code if the node has children
#' @export
node_multinomial <- function(data, parents, betas, class_labels=NULL) {
  # prep data
  mat <- as.matrix(data[, parents])

  # generate scores and sample from those
  probs <- cbind(apply(betas, MARGIN=1, FUN=function(x, mat){mat %*% x},
                       mat=mat))
  choice_mat <- t(apply(probs, 1, stats::rmultinom, n = 1, size = 1))
  out <- apply(choice_mat, 1, function(x) which(x==1))

  # turn into factor
  if (!is.null(class_labels)) {
    out <- factor(out, levels=class_labels)
  } else {
    out <- factor(out)
  }
  return(out)
}

## a node modeled using cox-regression
#' @export
node_cox <- function(data, parents, betas, surv_dist, lambda, gamma,
                     cens_dist, cens_args) {
  # generate survival times
  time <- apply(as.data.frame(data[, parents]), MARGIN=1,
                FUN=sim_surv_time, betas=betas, dist=surv_dist,
                lambda=lambda, gamma=gamma)

  # add censoring, if specified
  if (!is.null(cens_dist)) {
    cens_fun <- get(cens_dist)
    cens_time <- do.call(cens_fun, c(n=nrow(data), cens_args))
    status <- ifelse(time < cens_time, 1, 0)
    time <- ifelse(time < cens_time, time, cens_time)
  } else {
    status <- 1
  }
  return(data.frame(time=time, status=status))
}

## a node modeled using poisson-regression
#' @export
node_poisson <- function(data, parents, betas, intercept) {
  eff <- rowSums(mapply("*", as.data.frame(data[, parents]), betas))
  mu <- exp(intercept + eff)
  out <- vapply(mu, FUN=stats::rpois, FUN.VALUE=numeric(1), n=1)
  return(out)
}
