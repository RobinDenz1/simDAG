
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
node_cox <- function(data, parents, formula=NULL, betas, surv_dist,
                     lambda, gamma, cens_dist, cens_args, name,
                     as_two_cols=TRUE) {

  if (!data.table::is.data.table(data)) {
    data.table::setDT(data)
  }

  if (!is.null(formula)) {
    data <- stats::model.matrix(object=formula, data=data)
    data <- as.data.frame(data[, -1])
  } else {
    data <- as.data.frame(data[, parents, with=FALSE])
  }

  # generate survival times
  time <- apply(data, MARGIN=1,
                FUN=sim_surv_time, betas=betas, dist=surv_dist,
                lambda=lambda, gamma=gamma)

  if (!as_two_cols && is.null(cens_dist)) {
    out_data <- time
  } else {
    out_data <- add_censoring(times=time, cens_dist=cens_dist,
                              cens_args=cens_args, name=name)
  }

  return(out_data)
}

## add censoring to a survival time, if specified
add_censoring <- function(times, cens_dist, cens_args, name) {

  if (!is.null(cens_dist)) {

    if (!is.function(cens_dist)) {
      cens_dist <- get(cens_dist)
    }

    cens_time <- do.call(cens_dist, c(n=length(times), cens_args))
    status <- ifelse(times < cens_time, 1, 0)
    times <- ifelse(times < cens_time, times, cens_time)
  } else {
    status <- 1
  }

  # put together in two columns
  out_data <- data.table::data.table(time=times, status=status)
  colnames(out_data) <- c(paste0(name, "_time"), paste0(name, "_status"))

  return(out_data)
}
