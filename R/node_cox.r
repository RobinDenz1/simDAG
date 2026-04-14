
## simulate survival time according to Bender et al. (2005)
sim_surv_time <- function(data, betas, dist, lambda, gamma=NULL, left=0) {

  eff <- calc_linpred(data=data, betas=betas, intercept=0)
  U <- stats::runif(nrow(data))

  # cumulative baseline hazard at left
  if (dist == "weibull") {
    H0_left <- lambda * left^gamma
  } else if (dist == "exponential") {
    H0_left <- lambda * left
  }

  # conditional survival inversion
  H0_t <- H0_left - log(1 - U) / exp(eff)

  # invert cumulative baseline hazard
  if (dist == "weibull") {
    surv_time <- (H0_t / lambda)^(1 / gamma)
  } else if (dist == "exponential") {
    surv_time <- H0_t / lambda
  }

  return(surv_time)
}

## a node modeled using cox-regression
#' @export
node_cox <- function(data, parents, formula=NULL, betas, surv_dist,
                     lambda, gamma, cens_dist=NULL, cens_args, name,
                     as_two_cols=TRUE, left=0, basehaz_grid=NULL,
                     extrapolate=FALSE, as_integer=FALSE, ...) {

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
  if (is.function(surv_dist)) {
    time <- rtrunc_surv(data=data, betas=betas, fbasehaz=surv_dist,
                        times=basehaz_grid, left=left, extrapolate=extrapolate,
                        ...)
  } else {
    time <- sim_surv_time(data=data, betas=betas, dist=surv_dist, lambda=lambda,
                          gamma=gamma, left=left)
  }

  if (!as_two_cols && is.null(cens_dist)) {
    if (as_integer) {
      out_data <- ceiling(time)
    } else {
      out_data <- time
    }
  } else {
    out_data <- add_censoring(times=time, cens_dist=cens_dist,
                              cens_args=cens_args, name=name)
    if (as_integer) {
      out_data[,1] <- ceiling(out_data[,1])
    }
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

## given an arbitrary baseline probability function, calculate the
## cumulative baseline hazard over some discretized time grid
get_numeric_cum_hazard <- function(f, times) {

  # evaulate f(t) at each value in grid
  f_vals <- c(0, f(times))

  if (anyNA(f_vals) || any(f_vals < 0)) {
    stop("Hazards must be positive at all 'times'.", call.=FALSE)
  }

  # trapezoidal increments for each interval (t_{k-1}, t_k]
  increments <- (f_vals[-1] + f_vals[-length(f_vals)]) / 2

  H0 <- cumsum(increments)

  return(H0)
}

## convenience function to get the approximated cumulative baseline hazard
## and its inverse as approximate functions
get_lH0 <- function(f, times) {

  # calculate cum. baseline hazard over given time grid
  H0 <- get_numeric_cum_hazard(f=f, times=times)
  H0_fun <- stats::approxfun(x=times, y=H0, method="linear", rule=1)

  # invert cum. hazard function
  invH0 <- stats::approxfun(x=H0, y=times, method="linear", rule=2)

  out <- list(H0=H0_fun, invH0=invH0)
  return(out)
}

## generate random (left-truncated) survival times from a Cox model given
## covariates, betas and a custom baseline hazard function
rtrunc_surv <- function(data, betas, fbasehaz, times, left=0,
                        extrapolate=FALSE, lH0=NULL) {
  max_t <- max(times)

  if (is.null(lH0)) {
    lH0 <- get_lH0(f=fbasehaz, times=times)
  }

  # get cum. baseline hazard at left truncation times
  if (anyNA(left) || any(left > max_t)) {
    stop("Left truncation times must be smaller than max(times). Adjust",
         " the 'times' argument or the 'left' argument.", call.=FALSE)
  }
  H_l <- ifelse(left==0, 0, lH0$H0(left))

  # get exponentiated linear predictor
  eff <- exp(calc_linpred(data=data, betas=betas, intercept=0))

  # simulate cum. hazard for each individual
  target <- H_l + -log(stats::runif(nrow(data))) / eff

  if (!extrapolate && any(target > lH0$H0(max(times)))) {
    stop("Some randomly generated hazards go beyond the estimated cumulative",
         " baseline hazard as defined by the 'times' argument and will",
         " therefore be equal to max(times). Adjust the 'times' argument",
         " to include a longer time grid or set 'extrapolate=TRUE' to",
         " ignore this issue.", call.=FALSE)
  }

  # identify corresponding time from target
  times <- lH0$invH0(target)

  return(times)
}
