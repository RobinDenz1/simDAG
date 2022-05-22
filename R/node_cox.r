
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
